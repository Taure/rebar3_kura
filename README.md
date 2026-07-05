# rebar3_kura

Rebar3 plugin for [Kura](https://github.com/Taure/kura) — auto-generates migration files from schema changes and bootstraps new projects.

## Installation

Add the plugin to your `rebar.config`:

```erlang
{project_plugins, [
    {rebar3_kura, "~> 0.14"}
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

### `rebar3 kura check`

CI gate. Fails the build if your `kura_schema` modules have drifted from the migration history, without generating any files. Pair it with `compile` (which does the fix).

```
$ rebar3 kura check     # exit 1 with a drift report, or 0 if clean
```

### `rebar3 kura gen_schemas`

Bootstraps `kura_schema` modules from an existing PostgreSQL database (the `db pull` / `inspectdb` move). Introspects the catalog through your app's configured backend and emits schemas only, never migrations.

```
$ rebar3 kura gen_schemas
$ rebar3 kura gen_schemas --force    # overwrite existing schema files
$ rebar3 kura gen_schemas --strict   # abort on any unsupported column type
```

Catalog identifiers are allowlist-validated before codegen; unsupported types are skipped with a warning (or abort under `--strict`).

### `rebar3 kura lint_migrations`

CI gate that flags unsafe migration DDL (drop table/column, rename, in-place retype, `NOT NULL` without a default) before it reaches a running database. Runs kura's own unsafe-operation check over each migration's `up/0`.

```
$ rebar3 kura lint_migrations          # exit 1 on a finding, or 0 if clean
$ rebar3 kura lint_migrations --warn   # report only, always exit 0
```

Honours each migration's optional `safe/0` callback as a per-operation opt-out.

### `rebar3 kura gen_auth`

Generates authentication schema and migration scaffolding for a new project.

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
