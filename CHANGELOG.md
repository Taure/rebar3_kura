# Changelog

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
