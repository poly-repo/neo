![img](neo.png)

**Neo** isn’t a distribution and it’s not trying to compete with the big,
polished setups like Spacemacs or Doom.

It is probably not what you want, but in case you really wanted to make sure it
isn't, you can download NEO.

> [!WARNING]\
> **Standard Internet Disclaimer:** Running code directly from a URL is
> technically a "Bad Idea™." Here is everything `setup.sh` touches, so you don't
> have to take our word for it:
>
> - Clones (or pulls) this repository into `~/neo`.
> - On a fresh clone, temporarily clones `poly-repo/neo-extensions` and
>   `poly-repo/mav-extensions`, then installs their complete publisher trees at
>   `~/neo/extensions/extensions/neo` and
>   `~/neo/extensions/extensions/mav`.
> - Creates a Python virtual environment at `~/neo/.neo-python` and installs
>   `~/neo/requirements.txt` into it (this may `sudo apt install python3-venv`
>   if that package isn't already present). NEO helpers invoke this
>   environment directly; setup does not activate it globally in your shell.
>   It is separate from any general `~/.python` environment configured by VM
>   provisioning.
>
> That's the extent of what `setup.sh` itself does. It does **not** stop there
> for the lifetime of your NEO install, though:
>
> - Once you start Emacs, installing NEO extensions can fetch additional code
>   (from the extension registry) and, for extensions that declare tree-sitter
>   grammars, build and cache grammar binaries under your Emacs cache directory
>   (e.g. `~/.cache/neo/tree-sitter/...`) — which requires a C compiler.
> - If `setup.sh` doesn't find an `emacs` on your `PATH` newer than version
>   32.0, it offers to build one from master for you via the `emacs-install`
>   Ansible role and installs the configured font families through the separate
>   `fonts` role. Before doing anything it prints an itemized list of what
>   accepting will do, then asks for confirmation — nothing runs without an
>   explicit "yes" (and if `setup.sh` can't prompt you interactively, e.g. in
>   CI, it prints the equivalent commands and skips instead of guessing). This
>   goes well beyond `~/neo`: it lazily installs `ansible` into
>   `~/neo/.neo-python` if needed, `sudo apt install`s build dependencies via
>   your system package manager, builds and installs Emacs itself (under
>   `~/.local/emacs-master-<date>-gtk3`), installs Google and Nerd Font families
>   into the user's font directory, and — only if you separately confirm a
>   second prompt — installs a crontab entry to keep that build fresh daily.
>   Only opt into any of this if you're comfortable with Ansible making those
>   changes.
>
> If any of this does something undisclosed here, feel free to haunt our commit
> history forever.

```sh
curl -fsSL https://github.com/poly-repo/neo/releases/download/latest/setup.sh | bash
```

After that you should be able to run:

```sh
emacs --init-directory ~/neo
```

The same Ansible operations are also available independently:

```sh
~/neo/scripts/install-emacs          # master-gtk3
~/neo/scripts/install-emacs 30.2     # 30.2-gtk3
~/neo/scripts/install-fonts
```

These wrappers locate the NEO root themselves, so they can be invoked from any
working directory.

You need Emacs 30.2 or newer. Why? I don't know, I'm not testing with anything
older. `setup.sh`'s build offer kicks in below version 32.0, ahead of that 30.2
floor — that's to steer people toward a fresh master build, not because
30.2–32.0 doesn't work.

## Customize

Neo does not use Emacs Customize as a persistent configuration mechanism.

1. Remove or rename `~/.emacs` before starting Neo with `--init-directory`. A
   stale `~/.emacs` can still become `user-init-file` and bypass `init.el` in
   the Neo directory.
2. Start Neo with `emacs --init-directory ~/neo`.
3. Keep configuration in Neo source files or Neo-managed profile files instead
   of saving from Customize.
4. If you open a Customize buffer anyway, Neo discards persistence by pointing
   `custom-file` at `null-device`.
