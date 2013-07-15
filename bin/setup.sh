#!/bin/sh

ssh $@ <<EOF
git clone --recursive https://github.com/maoe/dotfiles.git
cd dotfiles
rake install
EOF
