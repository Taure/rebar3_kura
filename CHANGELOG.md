# Changelog

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
