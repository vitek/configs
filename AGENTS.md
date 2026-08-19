# Notes for coding agents

- Configs are installed by **copying** into `$HOME` via make, nothing is
  symlinked. To apply a change: edit the file **in this repo**, then run the
  matching install target and reload the app, e.g.
  `make install-sway && swaymsg reload`,
  `make install-tmux && tmux source ~/.tmux.conf`.
  Never edit files under `~/.config` directly — the next `make install-*`
  overwrites them. See README.md for the list of targets.
- The worktree contains many untracked scratch files (foo*, tmp/, logs,
  tarballs...). Never use `git add -A`/`git add .` — stage files explicitly.
- Sway config is split: `wayland/sway/config.d/*.conf` (shared) and
  `wayland/sway/hostname.d/$(hostname).conf` (per-machine, e.g. outputs).
  i3 follows the same pattern in `i3/`.
- Commit messages follow the `component: short description` style, e.g.
  `sway: enable compose key`.
- `make diff` shows how installed files in `$HOME` differ from the repo.
