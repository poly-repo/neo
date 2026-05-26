![img](neo.png)

**Neo** isn’t a distribution and it’s not trying to compete with the
big, polished setups like Spacemacs or Doom.

It is probably not what you want, but in case you really wanted to
make sure it isn't, you can download NEO.

> [!WARNING]
> **Standard Internet Disclaimer:** Running code directly from a URL
> is technically a "Bad Idea™." However, we **pinky promise** this
> script only touches your `~/neo` directory to set up your Emacs
> environment. If it does anything else, feel free to haunt our commit
> history forever.

``` sh
curl -fsSL https://github.com/poly-repo/neo/releases/download/latest/setup.sh | bash
```

After that you should be able to run:

``` sh
emacs --init-directory ~/neo
```

You need Emacs 30.2 or newer. Why? I don't know, I'm not testing with anything older.

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
