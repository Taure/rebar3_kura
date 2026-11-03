# Changelog

## [Unreleased]

### Changed

- `rebar3 kura gen_auth` generates nova_auth 0.2 scaffolding: a user schema (`email`, `hashed_password`, `confirmed_at`, hashing with `nova_auth_password:hash/1`), a user token schema (with `family_id` and `used_at` for refresh tokens) and a `nova_auth` config module. It no longer writes a migration (the compile hook generates it from the schemas) or an accounts module, and no longer refers to bcrypt. The generated changesets store the email normalised (NFC, lower case; `normalize_email/1` is exported for login), reject a trailing newline and invalid UTF-8, count the password in code points (12 to 1024), drop the plaintext password from the changeset whether it is valid or not, and reset `confirmed_at` when the email changes. The printed next steps confirm the address before a session is issued and change the email through `nova_auth_accounts:change_identity/4`
- `rebar3 kura check` is documented to run before anything that compiles: the compile hook writes the missing migration first, so a check after `rebar3 compile`, `eunit` or `ct` always passes. If your CI runs `kura check` after a compiling step, move it before, or fail the build when the step left changes (`git diff --exit-code` and no untracked files under `src/migrations`)
- `rebar3 kura setup` suggests `{kura, "~> 2.25"}` and `{kura_postgres, "~> 1.1"}`, and the generated repo module has no `start/0`: kura starts the pool from the `{repos, #{...}}` config at boot
- Generated migrations, and the files `setup` and `gen_auth` write, are written through erlfmt when the project lists it in `project_plugins` or `plugins`, with its `print_width` and pragma setting, so `rebar3 fmt --check` passes right after generation. An erlfmt that is only a dependency is not used
- An index declared in `indexes/0` with a `name` is created, dropped and rolled back under that name, so a rollback no longer fails with "index does not exist" and removing one of two indexes on a column drops that one. A changed `name`, or a change to `using`, `opclass` or `with` alone, generates a drop and a create of the index. An undeclared name matches the index under the generated name first, then any alike index whose name no declaration claims, so a column rename or a migration-chosen name is not drift. Default naming is unchanged. Earlier versions created a named index under the generated name, so the first diff drops such an index and recreates it under its declared name
- A `name` in `indexes/0` that is not a binary of 1 to 63 bytes aborts with `invalid_index_name`, as kura with Taure/kura#212 does, instead of being ignored. Two entries that resolve to the same index name, in any tables, abort with `duplicate_index_name`
- A named index with only `unique`, `where` and `concurrently` is generated as the 5-tuple `create_index`, which kura honours before Taure/kura#212, though only from kura 2.25.0 for `concurrently`. A named index with `using`, `opclass` or `with` is generated in the map form, which needs the app on kura 2.27.0 or later: older kura ignores the `name` and those options there and builds a plain btree under the generated name
- Requires kura `~> 2.28`. kura 2.24 added `kura_schema:forget/1`, so an edited `key/0` is read fresh in a running VM, and 2.27 honours a map-form index `name` and takes `using`, `opclass` and `with`
- New tables that reference each other in a cycle abort with `reference_cycle` instead of generating `create_table` ops no order can run (#41)
- A change to an existing table's set of primary key columns (a field gaining or losing `primary_key = true`, a new key field, a dropped key field) aborts with `primary_key_changed`, naming the table, the column and the expected key constraint, instead of generating nothing or an `add_column` that cannot run. Replay reads the hand-written `DROP CONSTRAINT` / `ADD PRIMARY KEY` that makes the change, and a table rebuild from `create_table/4` and `drop_table` for SQLite (#53)

### Fixed

- An embed declared only in `embeds/0` gets its `jsonb` column; it used to get none unless `fields/0` declared it too. A schema with such an embed gets an `add_column ... jsonb` migration on the next compile, and `rebar3 kura check` fails until that migration is committed. A field named like an embed keeps its type and nullability but not its `column`, matching what kura reads and writes
- A schema module that another schema embeds gets no table when it has no `table/0` or the `table() -> ~"_embedded"` placeholder older kura guides showed, even when it declares `-behaviour(kura_schema)`; `compile` and `check` warn by name. An embedded module naming any other table keeps it, with a warning
- `embeds/0` raising aborts with `embeds_failed`, naming the schema, as `associations/0` does
- Generated `up/0` creates a new table after the new tables it references, and `down/0` undoes every step of `up/0` in exactly the reverse order, across tables and columns, so a rollback no longer drops a table a new column or foreign key still references (#41)
- Replay reads `{create_table, Name, Columns, Constraints}`: the table was missing from the replayed state, so the next run generated a second `create_table` for it. A `primary_key` constraint marks its columns; a table's `unique` constraints are reported together, once, like an unreplayed `execute`, and `check` constraints, which no diff depends on, are not reported (#52)
- `concurrently` is not compared, so turning it on or off no longer drops and rebuilds an index. A new index keeps it
- Replay of a 5-tuple `create_index` keeps `concurrently`
- Replay of a `rename_column` rewrites every table's `references` to the renamed column, as Postgres does, so the next run no longer drops and re-adds those foreign keys (#54)
- The primary key is read through `kura_schema:key/1`, so a schema that declares it with `key/0` and no `primary_key = true` flags, kura's documented composite form, gets a keyed table and no false `primary_key_changed` refusal
- A new table whose schema declares a composite primary key is generated as `{create_table, Name, Columns, [{primary_key, Key}]}` rather than a `create_table/3` that declares `PRIMARY KEY` on every key column and fails. Its key columns are written `nullable = false`, because SQLite, unlike PostgreSQL, lets a composite key column hold NULL. A key column left nullable in an existing hand-written migration is not reported as drift: PostgreSQL already holds it NOT NULL and SQLite cannot `SET NOT NULL`, so on SQLite such a table needs the rebuild described in the README

## [0.6.0] - 2026-02-27

### Changed

- Bump kura dependency to `~> 1.2`

## [0.5.0] - 2026-02-14

### Added

- Makefile for test and quality checks (`make test`, `make check`)

### Changed

- Bump kura dependency to `~> 0.5` (embedded schemas, many-to-many, schemaless changesets)

## [0.4.2] - 2026-02-14

### Changed

- Updated README with setup command docs and current version
- Added ex_doc configuration for hex documentation

## [0.4.1] - 2026-02-14

### Changed

- Bump kura dependency to `~> 0.4`
- Remove dialyzer nowarn on `types_equal/2` (enum type now in kura PLT)

## [0.4.0] - 2026-02-14

### Added

- `types_equal/2` in `kura_schema_diff` to handle enum type equality
- Prevents no-op `ALTER TABLE` when only enum values change (both map to `VARCHAR(255)`)

## [0.3.0] - 2026-02-14

### Added

- `rebar3 kura setup` command for bootstrapping Kura in a project
- Generates repo module with all kura_repo_worker wrappers
- Creates `src/migrations/` directory
- Checks kura dependency and provider hook configuration
- `--name` flag to customize repo module name (default: `APPNAME_repo`)

### Changed

- Kura dependency switched from git to hex (`~> 0.3`)

## [0.2.0] - 2026-02-14

### Added

- Xref, dialyzer, and erlfmt checks
- PropEr property-based tests
- Migration rendering tests

## [0.1.0] - 2026-02-14

### Added

- Pre-compile provider that diffs kura_schema modules against existing migrations
- Auto-generates migration `.erl` files for new tables, added/dropped columns, and type changes
- Idempotent — no migration generated when schemas match migrations
- Timestamp collision avoidance
- Does not auto-generate destructive `drop_table` operations
