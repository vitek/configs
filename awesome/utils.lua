local awful = require("awful")

require("awful.dbus")
local dbus = dbus

local utils = {}

function utils.switch_tag(s, step)
    local tag = s.tags[s.selected_tag.index + step]
    if tag then
        tag:view_only()
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

function utils.round(value, div)
    local k = value / (div or 1.0)
    local f = math.floor(k)
    local c = math.ceil(k)

    if math.abs(k - f) < math.abs(k - c) then
        return f * div
    else
        return c * div
    end
end

-- force english
dbus.add_match("session", "interface='org.gnome.ScreenSaver'")
dbus.connect_signal(
   "org.gnome.ScreenSaver",
   function(...)
      local data = {...}
      if data[1].member == "ActiveChanged" then
         awesome.xkb_set_layout_group(0)
      end
   end
)
return utils
