local awful = require("awful")
local wibox = require("wibox")
local utils = require("utils")
local pulseaudio = require("apw.pulseaudio")
local pulseaudio_widget = require("apw.widget")

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
   --local cpuwidget = wibox.widget.textbox()

   local cpuwidget = wibox.widget.graph()
   cpuwidget:set_width(25)
   cpuwidget:set_background_color("#494B4F")
   cpuwidget:set_color("#ff8888")
   vicious.cache(vicious.widgets.cpu)
   vicious.register(cpuwidget, vicious.widgets.cpu, "$1", 1)
   awful.tooltip({
         objects = { cpuwidget },
         timer_function = function()
            local info = vicious.widgets.cpu()
            return string.format("CPU %d %%", info[1])
         end,
   })

   local memwidget = wibox.widget.graph()
   memwidget:set_width(25)
   memwidget:set_background_color("#494B4F")
   memwidget:set_color("#8888ff")
   vicious.cache(vicious.widgets.mem)
   vicious.register(memwidget, vicious.widgets.mem, "$1", 1)
   awful.tooltip({
         objects = { memwidget },
         timer_function = function()
            local info = vicious.widgets.mem()
            return string.format("Memory %d %%", info[1])
         end,
   })

   local volumewidget = pulseaudio_widget(pulseaudio, {
        mixer = "gnome-control-center sound"
   })
   local volumewidget_tooltip = awful.tooltip({
         objects = { volumewidget },
         timer_function = function()
            return string.format("Volume %d %%", pulseaudio.Volume * 100)
         end,
   })
   pulseaudio.register_callback(function()
         if volumewidget_tooltip:get_visible() then
            volumewidget_tooltip.timer_function()
         end
   end)

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
         wibox.container.margin(cpuwidget, 1),
         wibox.container.margin(memwidget, 1),
         s.mypromptbox,
      },
      nil,
      { -- Right widgets
         layout = wibox.layout.fixed.horizontal,
         volumewidget,
         mykeyboardlayout,
         wibox.widget.systray(),
         mytextclock,
         mylauncher,
         --s.mylayoutbox,
      },
   }
end
return mywibar
