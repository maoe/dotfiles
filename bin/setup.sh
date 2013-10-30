#!/bin/sh
DOTFILES_DIR=$HOME/dotfiles

ssh $@ <<EOF
type git || echo "You need git to proceed"
type rake || echo "You need rake to proceed"

if [ -d "$DOTFILES_DIR" ]; then
  cd $DOTFILES_DIR
  git pull
else
  git clone --recursive https://github.com/maoe/dotfiles.git $DOTFILES_DIR
fi
cd $DOTFILES_DIR
yes o | rake install
EOF
