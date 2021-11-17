# Tim Bovelander's dotfiles

_These are my dotfiles, there are many like it, but these ones are mine._

## Dependencies

- `fish`
- `git`

## Installation

Installation is done by running `.script/setup.sh`.

If `wget` and `bash` are installed, you can do this by running the following command:

```sh
wget https://raw.github.com/timbovelander/dotfiles/master/.scripts/setup.sh -O /tmp/setup.sh && bash /tmp/setup.sh
```

1. Checks out dotfiles and applies them to `$HOME` - **overrides** existing files.
2. Installs [nodenv](https://github.com/nodenv/nodenv) to `$HOME/.nodenv` with plugins: [node-build](https://github.com/nodenv/node-build), [nodenv-update](https://github.com/nodenv/nodenv-update) and [nodenv-default-packages](https://github.com/nodenv/nodenv-default-packages)
