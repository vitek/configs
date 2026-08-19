# configs

This is my public rc-files. Feel free to examine and use it.

## Installation

Configs are installed by **copying** files into `$HOME` with make — nothing is
symlinked. So after editing a file in this repo you need to reinstall it and
reload the affected app.

Common targets (see `Makefile` and `*/Makefile` for the full list):

```sh
make install            # basics: vcs, editors, misc
make install-all        # everything
make install-bashrc
make install-emacs
make install-kitty
make install-sway       # sway + waybar/wob/dunst/etc, lives in wayland/
```

Examples of reinstall + reload:

```sh
# sway
make install-sway && swaymsg reload

# tmux
make install-tmux && tmux source ~/.tmux.conf
```

`make diff` shows how the installed files in `$HOME` differ from the repo.

`make ssh-deploy SSH_HOSTNAME=somehost` installs the terminal-only subset
(editors, vcs, bashrc) to a remote host over ssh.
