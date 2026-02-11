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

