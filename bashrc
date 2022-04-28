# -*- mode: shell-script; -*-
# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# don't put duplicate lines in the history. See bash(1) for more options
# ... or force ignoredups and ignorespace
HISTCONTROL=ignoredups:ignorespace

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

unset LESS
if [ -t /etc/gentoo-release ]; then
    # make less more friendly for non-text input files, see lesspipe(1)
    [ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"
fi

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if  ! shopt -oq posix; then
    if [ -f /etc/bash_completion ]; then
        . /etc/bash_completion
    elif [ -f /usr/local/etc/bash_completion ]; then # FreeBSD case
        . /usr/local/etc/bash_completion

        if [ -f /usr/local/share/git-core/contrib/completion/git-completion.bash ]; then
            . /usr/local/share/git-core/contrib/completion/git-completion.bash
        fi
    fi
fi

shopt -s direxpand

# Nice editor for small configs and commit messages
export EDITOR=vim
export PAGER=less

for bcfile in ~/.bash.d/*; do
    if [ -a "$bcfile" ]; then
        source "$bcfile"
    fi
done

if [[ "$INSIDE_EMACS" = 'vterm' ]] \
    && [[ -n ~/.bashrc_vterm ]] \
    && [[ -f ~/.bashrc_vterm ]]; then
    source ~/.bashrc_vterm
fi

export LIBOVERLAY_SCROLLBAR=0
export GTK_OVERLAY_SCROLLING=0
# https://ubuntuforums.org/showthread.php?t=2390362
export QT_AUTO_SCREEN_SCALE_FACTOR=0

if [ "x$EMACS_SOCKET_NAME" != "x" ]; then
    alias e="emacsclient -r -n --socket-name $EMACS_SOCKET_NAME"
else
    alias e="emacsclient -r -n"
fi

if [ -f ~/.bashrc_local ]; then
    . ~/.bashrc_local
fi
