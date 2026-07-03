# kitty watcher — two features, loaded via the global `watcher` option:
#   1. Put the tab position "[i/n] title" into the OS window title.
#   2. Remember the keyboard layout per tab and drive sway to restore it on tab
#      switch. Tabs are internal to kitty and invisible to sway, so kitty itself
#      talks to sway. Preferred path is the i3ipc socket (fast, one persistent
#      connection); falls back to the swaymsg CLI where i3ipc isn't available.
import json
import subprocess

from kitty.fast_data_types import set_os_window_title

try:
    import i3ipc
except Exception:
    i3ipc = None

# layout index applied to brand-new tabs (0 == first in `xkb_layout`, i.e. us)
DEFAULT_LAYOUT = 0

_tab_layout = {}   # tab id       -> sway layout index
_last_tab = {}     # os_window id -> tab id last seen active
_conn = None       # cached i3ipc connection


# ---------------------------------------------------------------- title -----
def _update_title(boss, window):
    try:
        osw = getattr(window, "os_window_id", None)
        if not osw:
            return
        tm = boss.os_window_map.get(osw)
        if tm is None:
            return
        n = len(tm.tabs)
        i = tm.active_tab_idx + 1
        at = tm.active_tab
        title = ""
        if at is not None:
            aw = at.active_window
            title = (aw.title if aw is not None else "") or ""
        set_os_window_title(osw, f"[{i}/{n}] {title}".rstrip())
    except Exception:
        pass


# --------------------------------------------------------------- layout -----
def _drop_conn():
    global _conn
    _conn = None


def _get_conn():
    global _conn
    if i3ipc is None:
        return None
    if _conn is None:
        try:
            _conn = i3ipc.Connection(auto_reconnect=True)
        except Exception:
            _conn = None
    return _conn


def _sway_get_layout():
    conn = _get_conn()
    if conn is not None:
        try:
            for inp in conn.get_inputs():
                if getattr(inp, "type", None) == "keyboard" \
                        and getattr(inp, "xkb_active_layout_index", None) is not None:
                    return int(inp.xkb_active_layout_index)
            return None
        except Exception:
            _drop_conn()
    # fallback: swaymsg CLI
    try:
        out = subprocess.run(
            ["swaymsg", "-r", "-t", "get_inputs"],
            capture_output=True, text=True, timeout=0.5,
        ).stdout
        for inp in json.loads(out):
            if inp.get("type") == "keyboard" and "xkb_active_layout_index" in inp:
                return int(inp["xkb_active_layout_index"])
    except Exception:
        pass
    return None


def _sway_set_layout(idx):
    conn = _get_conn()
    if conn is not None:
        try:
            conn.command(f"input type:keyboard xkb_switch_layout {idx}")
            return
        except Exception:
            _drop_conn()
    # fallback: swaymsg CLI
    try:
        subprocess.Popen(
            ["swaymsg", "input", "type:keyboard", "xkb_switch_layout", str(idx)],
            stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
        )
    except Exception:
        pass


def _update_layout(boss, window):
    try:
        osw = getattr(window, "os_window_id", None)
        if not osw:
            return
        tm = boss.os_window_map.get(osw)
        if tm is None:
            return
        at = tm.active_tab
        if at is None:
            return
        cur_tab = getattr(at, "id", None) or id(at)
        prev_tab = _last_tab.get(osw)
        if prev_tab == cur_tab:
            return  # focus moved but same tab -> nothing to do

        cur_layout = _sway_get_layout()

        if prev_tab is None:
            # first time we see this window: adopt the current layout for this
            # tab, do not force anything on the user
            if cur_layout is not None:
                _tab_layout.setdefault(cur_tab, cur_layout)
            _last_tab[osw] = cur_tab
            return

        # real tab switch: save the layout the user had in the tab being left
        if cur_layout is not None:
            _tab_layout[prev_tab] = cur_layout
        # and restore the layout remembered for the tab being entered
        target = _tab_layout.get(cur_tab, DEFAULT_LAYOUT)
        if cur_layout is None or target != cur_layout:
            _sway_set_layout(target)
        _last_tab[osw] = cur_tab
    except Exception:
        pass


# --------------------------------------------------------------- events -----
def on_focus_change(boss, window, data):
    _update_layout(boss, window)
    _update_title(boss, window)


def on_title_change(boss, window, data):
    _update_title(boss, window)


def on_load(boss, window, data):
    _update_layout(boss, window)
    _update_title(boss, window)


def on_close(boss, window, data):
    _update_title(boss, window)
