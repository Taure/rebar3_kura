# rebar3_kura

Rebar3 plugin for [Kura](https://github.com/Taure/kura) — auto-generates migration files from schema changes and bootstraps new projects.

## Installation

Add the plugin to your `rebar.config`:

```erlang
{project_plugins, [
    {rebar3_kura, "~> 0.4"}
]}.

{provider_hooks, [
    {pre, [{compile, {kura, compile}}]}
]}.
```

## Commands

### `rebar3 kura compile`

Runs automatically as a pre-compile hook. Diffs your `kura_schema` modules against existing migrations and generates new migration files when changes are detected.

```
$ rebar3 compile
===> kura: generated src/migrations/m20260214120000_create_pets.erl
```

### `rebar3 kura setup`

Bootstraps Kura in a new project:

```
$ rebar3 kura setup
$ rebar3 kura setup --name my_db  # custom repo module name
```

This will:
1. Check that kura is in your deps
2. Generate a repo module (`APPNAME_repo.erl`) with all `kura_repo_worker` wrappers
3. Create the `src/migrations/` directory
4. Check that the provider hook is configured
5. Print remaining manual setup steps

## Auto-migration

The pre-compile hook detects:

- **New tables** — schema exists, no migration history
- **Added columns** — field in schema, not in migration history
- **Dropped columns** — column in migration history, not in schema
- **Type changes** — same column name, different type

It does **not** auto-generate:

- `drop_table` operations (destructive — write manually)
- Nullable-only changes
- Default value changes
- Primary key changes

### Enum handling

Enum fields (`{enum, [atom()]}`) map to `VARCHAR(255)`. Changing the enum value list doesn't trigger a migration since the underlying column type is unchanged.

## How it works

1. Change a schema (e.g., add a field to `pet.erl`)
2. Run `rebar3 compile`
3. The pre-compile hook diffs schemas against existing migrations
4. If changes are detected, a new migration `.erl` file is generated in `src/migrations/`
5. The main compile step compiles everything including the new migration
6. Next `rebar3 compile` — no diff, no new migration (idempotent)

## License

MIT
