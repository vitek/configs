# -*- mode: shell-script; -*-

if [ "$TERM" == "dumb" -o "$TERM" == "tramp" ]; then
    return
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
