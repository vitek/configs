# pyright: reportMissingImports=false
# Minimal tab bar: fixed-width " N " slot per tab, active one highlighted.
# Every tab reserves the same padding (one cell on each side of the number),
# so the layout does not shift when the active tab changes and the active box
# is symmetric; only the background color differs.

from kitty.fast_data_types import Screen
from kitty.tab_bar import DrawData, ExtraData, TabBarData, as_rgb
from kitty.utils import color_as_int


def draw_tab(
    draw_data: DrawData,
    screen: Screen,
    tab: TabBarData,
    before: int,
    max_title_length: int,
    index: int,
    is_last: bool,
    extra_data: ExtraData,
) -> int:
    # match terminal text weight: never bold/italic
    screen.cursor.bold = False
    screen.cursor.italic = False

    if tab.is_active:
        screen.cursor.bg = as_rgb(color_as_int(draw_data.active_bg))
        screen.cursor.fg = as_rgb(color_as_int(draw_data.active_fg))
    else:
        screen.cursor.bg = as_rgb(color_as_int(draw_data.inactive_bg))
        screen.cursor.fg = as_rgb(color_as_int(draw_data.inactive_fg))

    # fixed-width slot: one padding cell on each side of the number
    screen.draw(f" {index} ")

    return screen.cursor.x
