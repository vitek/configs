# -*- mode: shell-script; -*-
# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
        . "$HOME/.bashrc"
    fi
else
    ENV=$HOME/.shrc; export ENV
fi

PATH="$PATH:$HOME/bin:$HOME/.local/bin"

if [ "x$XDG_RUNTIME_DIR" != "x" ]; then
    export EMACS_SOCKET_NAME=$XDG_RUNTIME_DIR/emacs/server
    export SSH_AUTH_SOCK=$XDG_RUNTIME_DIR/keyring/ssh
fi
