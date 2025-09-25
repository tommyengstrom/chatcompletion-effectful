# Repository Guidelines

## Project Structure & Module Organization
The library source lives under `src/ChatCompletion` with domain-specific modules for effects, storage backends (`Storage/InMemory.hs`, `Storage/Postgres.hs`), providers (`Providers/Google.hs`), and shared types. `src/Effectful/OpenAI.hs` wraps the OpenAI bindings. Tests in `test/` mirror the library layout, with `test/ChatCompletion/...Spec.hs` covering each backend and `test/ProviderAgnosticTests.hs` exercising the public surface. Keep generated artefacts (`chatcompletion-effectful.cabal`) in sync by updating `package.yaml`; `hpack` emits the cabal file. Helpers such as `build.sh` and `run-ghciwatch.sh` sit at the repo root.

## Build, Test, and Development Commands
- `hpack` regenerates `chatcompletion-effectful.cabal` from `package.yaml`; run it before builds when dependencies change.
- `./build.sh` performs `hpack` and `cabal build all` for a clean CI-aligned build.
- `cabal build` or `cabal build chatcompletion-effectful` covers incremental library compiles.
- `cabal repl chatcompletion-effectful:test:test-dev` (or `./run-ghciwatch.sh`) starts a live test REPL with automatic reloads.
- `cabal test chatcompletion-effectful-test` runs the released Hspec suite; `cabal test test-dev` includes src files for exploratory specs.

## Coding Style & Formatting
We rely on Fourmolu with the repo-local `fourmolu.yaml` (4-space indents, 90-column limit, leading commas/arrows). Format changes with `fourmolu --mode inplace $(git ls-files '*.hs')` before committing. Modules compile with `-Weverything -Werror`, so eliminate warnings, keep imports explicit, and prefer `Relude` utilities. Leave `.cabal` edits to `hpack`; hand edits are overwritten.

## Testing Guidelines
Specs use Hspec (`hspec-discover` expects `*Spec.hs` modules exporting `spec`). Postgres-related specs connect to `host=localhost port=5432 user=postgres password=postgres dbname=chatcompletion-test`; ensure that database exists or skip with `--test-options '--match /InMemory/'` when offline. Add focused specs beside the module under test and share fixtures via `test/Main/`.

## Commit & PR Workflow
Current history favors short, present-tense subjects (e.g. "reexport chatcompletion") with optional body details. Keep commits scoped, include test or migration notes when touching storage. PRs should summarise intent, list impacted modules, call out schema or config changes, and confirm `fourmolu` + `cabal test` ran. Attach logs or screenshots if behaviour changes.
