#!/bin/sh
set -e

PACKAGES="git haskell rc ruby starship tig tmux vim zsh"

for pkg in $PACKAGES; do
    stow -v --target="$HOME" "$pkg"
done
