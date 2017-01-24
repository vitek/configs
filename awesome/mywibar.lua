local awful = require("awful")
local wibox = require("wibox")
local utils = require("utils")
local APW = require("apw/widget")

local mywibar = {}

-- Create a wibox for each screen and add it
-- @TAGLIST_BUTTON@
local taglist_buttons = awful.util.table.join(
                    awful.button({ }, 1, function(t) t:view_only() end),
                    awful.button({ modkey }, 1, function(t)
                                              if client.focus then
                                                  client.focus:move_to_tag(t)
                                              end
                                          end),
                    awful.button({ }, 3, awful.tag.viewtoggle),
                    awful.button({ modkey }, 3, function(t)
                                              if client.focus then
                                                  client.focus:toggle_tag(t)
                                              end
                                          end),
                    awful.button({ }, 4,
                       function(t) utils.switch_tag(t.screen, -1) end),
                    awful.button({ }, 5,
                       function(t) utils.switch_tag(t.screen, 1) end)
                )

-- Create a textclock widget
local mytextclock = wibox.widget.textclock(" %a %b %d, %H:%M:%S ", 1)

-- Keyboard map indicator and switcher
local mykeyboardlayout = awful.widget.keyboardlayout()

function mywibar.create_wibar(s)

   local vicious = require("vicious")
   local cpuwidget = wibox.widget.textbox()
   vicious.register(cpuwidget, vicious.widgets.cpu, " cpu: $1% ")

   local memwidget = wibox.widget.textbox()
   vicious.register(memwidget, vicious.widgets.mem, " mem: $1% ")


   -- Create a promptbox for each screen
   s.mypromptbox = awful.widget.prompt()
    -- Create an imagebox widget which will contains an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    s.mylayoutbox = awful.widget.layoutbox(s)
    s.mylayoutbox:buttons(awful.util.table.join(
                           awful.button({ }, 1, function () awful.layout.inc( 1) end),
                           awful.button({ }, 3, function () awful.layout.inc(-1) end),
                           awful.button({ }, 4, function () awful.layout.inc( 1) end),
                           awful.button({ }, 5, function () awful.layout.inc(-1) end)))
    -- Create a taglist widget
    s.mytaglist = awful.widget.taglist(s, awful.widget.taglist.filter.all, taglist_buttons)

    -- @DOC_WIBAR@
    -- Create the wibox
    s.mywibox = awful.wibar({ position = "top", screen = s })

    -- @DOC_SETUP_WIDGETS@
    -- Add widgets to the wibox
    s.mywibox:setup {
        layout = wibox.layout.align.horizontal,
        { -- Left widgets
            layout = wibox.layout.fixed.horizontal,
            s.mytaglist,
            cpuwidget,
            memwidget,
            s.mypromptbox,
        },
        nil,
        { -- Right widgets
           layout = wibox.layout.fixed.horizontal,
           APW,
           mykeyboardlayout,
           wibox.widget.systray(),
           mytextclock,
           mylauncher,
           --s.mylayoutbox,
        },
    }
end
return mywibar
