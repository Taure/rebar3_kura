# Changelog

## [0.1.0] - 2026-02-14

### Added

- Pre-compile provider that diffs kura_schema modules against existing migrations
- Auto-generates migration `.erl` files for new tables, added/dropped columns, and type changes
- Idempotent — no migration generated when schemas match migrations
- Timestamp collision avoidance
- Does not auto-generate destructive `drop_table` operations
