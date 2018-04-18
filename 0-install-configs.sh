#!/bin/sh

set -x

cd
rm .bashrc
ln -s scripts/.bashrc
ln -s scripts/.dejagnurc
ln -s scripts/dejagnu/
ln -s scripts/.emacs
ln -s scripts/.muttrc
ln -s scripts/.tmux.conf
ln -s scripts/mailcap

set +x
