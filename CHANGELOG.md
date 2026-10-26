# Changelog

## [Unreleased]

### Changed

- Requires kura `~> 2.22`, for `kura_schema:forget/1`, so an edited `key/0` is read fresh in a running VM
- New tables that reference each other in a cycle abort with `reference_cycle` instead of generating `create_table` ops no order can run (#41)
- A change to an existing table's set of primary key columns (a field gaining or losing `primary_key = true`, a new key field, a dropped key field) aborts with `primary_key_changed`, naming the table, the column and the expected key constraint, instead of generating nothing or an `add_column` that cannot run. Replay reads the hand-written `DROP CONSTRAINT` / `ADD PRIMARY KEY` that makes the change, and a table rebuild from `create_table/4` and `drop_table` for SQLite (#53)

### Fixed

- Generated `up/0` creates a new table after the new tables it references, and `down/0` undoes every step of `up/0` in exactly the reverse order, across tables and columns, so a rollback no longer drops a table a new column or foreign key still references (#41)
- Replay reads `{create_table, Name, Columns, Constraints}`: the table was missing from the replayed state, so the next run generated a second `create_table` for it. A `primary_key` constraint marks its columns; a table's `unique` constraints are reported together, once, like an unreplayed `execute`, and `check` constraints, which no diff depends on, are not reported (#52)
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
