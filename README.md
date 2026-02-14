# rebar3_kura

Rebar3 plugin that auto-generates [Kura](https://github.com/Taure/kura) migration files from schema changes.

## Usage

Add the plugin to your `rebar.config`:

```erlang
{project_plugins, [
    {rebar3_kura, "~> 0.1"}
]}.

{provider_hooks, [
    {pre, [{compile, {kura, compile}}]}
]}.
```

## How it works

1. Change a schema (e.g., add a field to `pet.erl`)
2. Run `rebar3 compile`
3. The pre-compile hook diffs schemas against existing migrations
4. If changes are detected, a new migration `.erl` file is generated in `src/migrations/`
5. The main compile step compiles everything including the new migration
6. Next `rebar3 compile` — no diff, no new migration (idempotent)

## What it detects

- New tables (schema exists, no migration history)
- New columns (field in schema, not in migration history)
- Dropped columns (column in migration history, not in schema)
- Type changes (same column name, different type)

## What it does NOT auto-generate

- `drop_table` operations (destructive — write these manually)
- Nullable-only changes
- Default value changes
- Primary key changes

## License

MIT
