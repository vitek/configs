local awful = require("awful")

require("awful.dbus")
local dbus = dbus

local utils = {}

function utils.switch_tag(screen, step)
   -- switch w/o cycle
   local sel = screen.selected_tag
   local tags = screen.tags
   for k, t in ipairs(tags) do
      if t == sel then
         local tag = screen.tags[k + step]
         if tag then
            tag:view_only()
         end
      end
   end
end

function utils.lock_screen()
   awful.spawn("gnome-screensaver-command --lock")
end

function utils.quit_session()
   awful.spawn("gnome-session-quit")
end

function utils.suspend()
   awful.spawn("systemctl -i suspend")
end

-- force english
dbus.add_match("session", "interface='org.gnome.ScreenSaver'")
dbus.connect_signal(
   "org.gnome.ScreenSaver",
   function(...)
      local data = {...}
      if data[1].member == "ActiveChanged" then
         print('locking')
         awesome.xkb_set_layout_group(0)
      end
   end
)
return utils
