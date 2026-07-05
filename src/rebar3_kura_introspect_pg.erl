-module(rebar3_kura_introspect_pg).
-moduledoc """
PostgreSQL catalog introspection for `rebar3 kura gen_schemas`.

Pure logic: it produces the catalog SQL and parses the returned rows into
a list of table descriptions, mapping each column's PG type to a kura
field type via `kura_types:from_pg_type/2`. The provider owns the DB
connection and feeds the rows in, so everything here is testable without
a database.

Table and column names that are not safe Erlang/kura identifiers
(`^[a-z_][a-z0-9_]*$`) are refused: an unsafe table is dropped whole, an
unsafe column lands in `skipped`. This keeps a crafted catalog identifier
from ever reaching code or path generation.
""".

-export([columns_query/0, constraints_query/0, build_tables/2, valid_ident/1]).

-doc "SQL selecting one row per column across the public schema.".
-spec columns_query() -> string().
columns_query() ->
    "SELECT table_name, column_name, udt_name, character_maximum_length, "
    "is_nullable, column_default "
    "FROM information_schema.columns "
    "WHERE table_schema = 'public' "
    "ORDER BY table_name, ordinal_position".

-doc "SQL selecting primary-key and foreign-key columns across the public schema.".
-spec constraints_query() -> string().
constraints_query() ->
    "SELECT tc.table_name, kcu.column_name, tc.constraint_type, "
    "ccu.table_name AS foreign_table "
    "FROM information_schema.table_constraints tc "
    "JOIN information_schema.key_column_usage kcu "
    "  ON tc.constraint_name = kcu.constraint_name "
    "  AND tc.table_schema = kcu.table_schema "
    "LEFT JOIN information_schema.constraint_column_usage ccu "
    "  ON tc.constraint_name = ccu.constraint_name "
    "  AND tc.constraint_type = 'FOREIGN KEY' "
    "WHERE tc.table_schema = 'public' "
    "  AND tc.constraint_type IN ('PRIMARY KEY', 'FOREIGN KEY')".

-doc "Return `true` if `Name` is a safe lowercase Erlang/kura identifier.".
-spec valid_ident(binary()) -> boolean().
valid_ident(Name) when is_binary(Name) ->
    byte_size(Name) =< 63 andalso
        re:run(Name, "^[a-z_][a-z0-9_]*$", [{capture, none}]) =:= match;
valid_ident(_) ->
    false.

-doc """
Merge column rows and constraint rows into a list of table descriptions.

Each column row is `{Table, Column, UdtName, CharMaxLen, IsNullable, Default}`;
each constraint row is `{Table, Column, ConstraintType, ForeignTable}`. Returns
`[#{table => binary(), fields => [field()], assocs => [assoc()], skipped => [skip()]}]`
where a field's type comes from `kura_types:from_pg_type/2`, columns of an
unsupported or unsafe type land in `skipped`, and a foreign-key column with a
safe target yields a `belongs_to` assoc. Tables with an unsafe name are dropped.
""".
-spec build_tables([tuple()], [tuple()]) -> [map()].
build_tables(ColumnRows, ConstraintRows) ->
    Pks = pk_columns(ConstraintRows),
    Fks = fk_columns(ConstraintRows),
    [
        build_table(Table, Cols, Pks, Fks)
     || {Table, Cols} <- group_by_table(ColumnRows), valid_ident(Table)
    ].

build_table(Table, Cols, Pks, Fks) ->
    TablePks = maps:get(Table, Pks, []),
    TableFks = maps:get(Table, Fks, #{}),
    {Fields, Skipped} = build_fields(Table, Cols, TablePks),
    Assocs = [
        {belongs_to, singular(FkTable), FkTable, Column}
     || {Column, FkTable} <- maps:to_list(TableFks),
        valid_ident(Column),
        valid_ident(FkTable)
    ],
    #{table => Table, fields => Fields, assocs => Assocs, skipped => Skipped}.

build_fields(Table, Cols, TablePks) ->
    build_fields(Table, Cols, TablePks, [], []).

build_fields(_Table, [], _TablePks, Fields, Skipped) ->
    {lists:reverse(Fields), lists:reverse(Skipped)};
build_fields(Table, [{_T, Column, Udt, CharLen, IsNullable, Default} | Rest], TablePks, Fs, Sk) ->
    case valid_ident(Column) of
        false ->
            build_fields(Table, Rest, TablePks, Fs, [{Table, Column, ~"unsafe identifier"} | Sk]);
        true ->
            Opts = #{serial => is_serial(Default), char_max_length => char_len(CharLen)},
            case kura_types:from_pg_type(Udt, Opts) of
                {unsupported, TypeName} ->
                    build_fields(Table, Rest, TablePks, Fs, [{Table, Column, TypeName} | Sk]);
                Type ->
                    Field = #{
                        name => Column,
                        type => Type,
                        primary_key => lists:member(Column, TablePks),
                        nullable => IsNullable =:= ~"YES"
                    },
                    build_fields(Table, Rest, TablePks, [Field | Fs], Sk)
            end
    end.

group_by_table(ColumnRows) ->
    Tables = distinct_tables(ColumnRows, []),
    [{T, [Row || {T2, _, _, _, _, _} = Row <- ColumnRows, T2 =:= T]} || T <- Tables].

distinct_tables([], Acc) ->
    lists:reverse(Acc);
distinct_tables([{T, _, _, _, _, _} | Rest], Acc) ->
    case lists:member(T, Acc) of
        true -> distinct_tables(Rest, Acc);
        false -> distinct_tables(Rest, [T | Acc])
    end.

pk_columns(Rows) ->
    pk_columns(Rows, #{}).

pk_columns([], Acc) ->
    Acc;
pk_columns([{Table, Column, ~"PRIMARY KEY", _} | Rest], Acc) ->
    pk_columns(Rest, Acc#{Table => [Column | maps:get(Table, Acc, [])]});
pk_columns([_ | Rest], Acc) ->
    pk_columns(Rest, Acc).

fk_columns(Rows) ->
    fk_columns(Rows, #{}).

fk_columns([], Acc) ->
    Acc;
fk_columns([{Table, Column, ~"FOREIGN KEY", ForeignTable} | Rest], Acc) when
    ForeignTable =/= undefined, ForeignTable =/= null
->
    Inner = maps:get(Table, Acc, #{}),
    fk_columns(Rest, Acc#{Table => Inner#{Column => ForeignTable}});
fk_columns([_ | Rest], Acc) ->
    fk_columns(Rest, Acc).

is_serial(Default) when is_binary(Default) ->
    binary:match(Default, ~"nextval(") =/= nomatch;
is_serial(_) ->
    false.

char_len(N) when is_integer(N) -> N;
char_len(_) -> undefined.

singular(Table) ->
    case byte_size(Table) > 1 andalso binary:last(Table) =:= $s of
        true -> binary:part(Table, 0, byte_size(Table) - 1);
        false -> Table
    end.
