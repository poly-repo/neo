# Testing NEO

NEO has two layers of testing:

1. **Buttercup specs** — unit-level tests (mocked I/O, no real Emacs UI, no real
   beads/git state), run via Bazel. This is what CI runs today.
2. **Live-boot smoke test** — actually starting a real NEO instance
   (`early-init.el` → `init.el` → extension load → app registration) and driving
   it interactively or in `--batch`. **There is currently no automation for
   this** (`.github/workflows/neo-core.yaml`'s `validate-early-init` job exists
   but all its steps are commented out — see omega-ji3u). Until that's wired up,
   this has to be done by hand, which is what this document covers.

## Running the buttercup specs

```sh
bazel test //devex/editors/emacs/extensions/extensions/neo/<extension>:all
```

or target an individual spec file the same way you'd target any other
`bazel test` — check for an existing `o_*`/buttercup rule wrapper under
`build/bzl/` before reaching for a raw rule.

## Running a live-boot smoke test manually

The goal is to boot a real NEO instance without touching your actual profile
(`~/.config/neo`, `~/.config/neo-devel`, `~/.cache/neo`, …), your real beads
workspace, or your real worktrees directory. This is done by picking a throwaway
instance name and pointing every piece of shared state at a scratch directory
instead.

### 1. Pick a throwaway instance name

`neo/get-emacs-instance-name` (`core/neo-early-init-utils.el`) resolves the
instance name from the `EMACS_NAME` env var first, before falling back to
`--name`/`-name` or the `"neo"` default. Setting `EMACS_NAME` is the simplest
way to get a fully isolated `neo/config-directory` (`$XDG_CONFIG_HOME/<name>`)
and `neo/cache-directory` (`$XDG_CACHE_HOME/<name>`) — including the
per-instance SQLite config DB, elpaca package cache, and eln-cache — without
colliding with any real, possibly-running instance.

```sh
export EMACS_NAME=neo-workflow-smoketest
export REPO=/path/to/devex/editors/emacs   # this directory
export XDG_CONFIG_HOME=/tmp/neo-smoketest/config
export XDG_CACHE_HOME=/tmp/neo-smoketest/cache
mkdir -p "$XDG_CONFIG_HOME" "$XDG_CACHE_HOME"
```

The first boot of a fresh instance name bootstraps elpaca from scratch (cloning
elpaca itself, downloading MELPA/NonGNU/GNU ELPA recipe archives, then
cloning/compiling every declared package) — this can take several minutes.
Subsequent boots of the same instance name reuse the cache and are fast.

### 2. Sandbox any extension you're testing that touches shared state

Check whether the extension under test reads real shared directories via env
vars or defcustoms, and override them:

- **beads** (`programming-foundation/beads/beads-client.el`): workspace
  resolution order is the `beads-dir` defcustom → `BEADS_DIR` env var →
  `BEADS_DB` env var → directory-walk upward from `default-directory` for
  `.beads/` → `bd where --json` CLI fallback. To avoid touching your real beads
  DB, either `unset BEADS_DIR BEADS_DB` and run Emacs with `default-directory`
  inside a scratch git repo that has its own `bd init`-created `.beads/`, or set
  `BEADS_DIR`/`BEADS_DB` to point at a scratch workspace explicitly.
- **neo-workflow worktrees** (`neo-workflow-status.el`): the `h`/activate action
  creates git worktrees under `neo/workflow-worktrees-directory`, which defaults
  to the real `~/Projects/worktrees`. Override it via `--eval` before exercising
  that code path (see step 4).
- **project discovery** (`projects/neo-projects.el`): `neo:projects` (a
  transitive dependency of `neo:neo-workflow`) hardcodes
  `projectile-project-search-path` to `~/Projects/`, `~/.local/share/wtrees`,
  and `~/Projects/worktrees/omega--m-vitale` — real, potentially large
  directories, not overridable via a defcustom. This is read-only (just
  Projectile's project-list scan), but on a big monorepo checkout it can take
  several minutes on every boot that loads `neo:projects`, on top of any elpaca
  bootstrap time. There's no sandboxing for this today; budget wall-clock time
  accordingly and don't mistake it for a hang.

### 3. Seed the config DB so your extension actually loads

`neo/bootstrap` (`core/neo-framework.el`) reads the `"enabled-extensions"` key
from the per-instance config DB and topo-sorts+auto-expands its transitive
`:requires`/`:depends-on` closure — so seeding just the leaf extension's slug
(`"<publisher>:<normalized-name>"`, e.g. `"neo:neo-workflow"`) is enough to pull
in its whole dependency chain. A fresh instance also starts with `neo/first-run`
true (routes to `display-startup-screen` instead of running `neo--startup`, so
extensions never load) unless overridden.

`early-init.el` itself `require`s `neo-config` and computes `neo/first-run`, so
both can be set right after loading `early-init.el`, before `init.el` runs:

```sh
emacs --batch --debug-init \
  -l "$REPO/early-init.el" \
  --eval '(neo/set-config "enabled-extensions" (prin1-to-string (list "neo:neo-workflow")))' \
  --eval '(setq neo/first-run nil)' \
  -l "$REPO/init.el" \
  -l /tmp/my-smoketest-driver.el
```

Notes:

- `--debug-init` sets `init-file-debug`, which `neo/debug-p` consults to decide
  whether to suppress `message` output (`inhibit-message`) during startup.
  Without it, all startup messages — including load errors reported via
  `message` rather than a signal — are silently swallowed, even though the
  process still exits 0.
- `early-init.el` must be loaded before `init.el` in batch mode: unlike
  interactive startup, `emacs --batch -l init.el` does not auto-source
  `early-init.el` first, and `init.el` immediately hits
  `Symbol's value as variable is void: neo/cache-directory` without it.
- A long first-time elpaca bootstrap will exceed short timeouts; launch it
  backgrounded (`nohup ... & disown`) and poll a log file rather than blocking
  on it synchronously.

### 4. Drive the instance from a driver script

`-l /tmp/my-smoketest-driver.el` above loads an arbitrary elisp file after
`neo--startup` has run, which is where you exercise the actual behavior under
test. Useful entry points:

- `M-a` is bound to `neo/applications-map` (`core/neo-application.el`) — **not**
  `neo/application-map`. Each `(neo/application "Name" :bind KEY ...)` form
  (e.g. `neo-workflow-status.el`'s `:bind "w"`) registers a key in that map;
  look up an app's binding with `(lookup-key neo/applications-map (kbd "w"))` or
  just call its `:setup` function directly (e.g. `(neo/workflow-status)`).
- Override shared-state defcustoms before exercising write paths, e.g.:

  ```elisp
  (setq neo/workflow-worktrees-directory "/tmp/neo-smoketest/worktrees")
  ```

- vtable row actions (`h`, `a`, `TAB`, `RET`, …) are invoked with the row's
  object as an argument, not via simulated keypresses. From a driver script,
  move point into the table, grab the object, and call the handler function
  directly:

  ```elisp
  (with-current-buffer "*NEO Workflow*"
    (goto-char (point-min))
    ;; move point to the target row, then:
    (let ((obj (vtable-current-object)))
      (neo--hack obj)))
  ```

- Wrap each check in `condition-case` and print a pass/fail line per assertion
  so a single failure doesn't stop the rest of the driver from reporting.

### 5. Clean up

Delete the scratch directories (`XDG_CONFIG_HOME`/`XDG_CACHE_HOME` roots, any
scratch git/beads repo, any scratch worktrees dir) and any temp driver files
afterward. None of this should ever leave a `workflow.sqlite` behind —
neo-workflow's board is derived live from the beads workspace and git, not
cached to its own DB; if one appears, that's a bug.

## Known gaps

- No CI automation for the live-boot smoke test — see omega-ji3u.
