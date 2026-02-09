# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

Personal dotfiles repository for Mitsutoshi Aoe. Configuration files are managed with [GNU Stow](https://www.gnu.org/software/stow/).

## Commands

```bash
./install.sh                  # Stow all packages into ~
stow -v --target="$HOME" zsh  # Stow a single package
stow -D zsh                   # Unstow (remove symlinks for) a package
bin/setup-dotfiles <host>     # Install dotfiles on a remote host via SSH
```

Prerequisites: `git`, `stow`.

## Stow Convention

Each top-level directory is a Stow "package". Files inside mirror their `$HOME` location and are named as real dotfiles:

- `git/.gitconfig` → `~/.gitconfig`
- `zsh/.zshrc` → `~/.zshrc`
- `tmux/.tmux.conf` → `~/.tmux.conf`

To add a new dotfile: place it at `<topic>/.<name>` and run `stow <topic>`.

## Architecture

**Topic-based layout** — each directory groups configs for one tool/domain (git, zsh, tmux, vim, haskell, ruby, tig, rc, gnome).

**Zsh loading order:**
1. `~/.zprofile` (login shell env)
2. `~/.zshrc` (main shell config, completions, aliases)
3. `~/.zshrc.mine` → sources `~/.zshrc.antigen` → platform file (`~/.zshrc.mac` or `~/.zshrc.linux`)
4. `~/.zshrc.private` (not tracked — personal overrides)

**Git submodules** in `modules/`: antigen (zsh plugin manager), git-flow-completion.

**Private/local config** (not tracked in this repo):
- `~/.gitconfig.private` — included via git's `[include]` directive
- `~/.zshrc.private` — sourced at end of zshrc
- `~/.zprofile.local` — sourced at end of zprofile

**Auto-update**: `zshrc.mine` checks `~/dotfiles/.touch` and pulls from origin if older than 7 days.
