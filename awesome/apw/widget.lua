-- Copyright 2013 mokasin
-- This file is part of the Awesome Pulseaudio Widget (APW).
--
-- APW is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- APW is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with APW. If not, see <http://www.gnu.org/licenses/>.

-- Configuration variables
local mixer         = 'pavucontrol' -- mixer command
local theme_icons = {
   ["audio-volume-high"] =
      "/usr/share/icons/ubuntu-mono-dark/status/22/audio-volume-high-panel.svg",
   ["audio-volume-medium"] =
      "/usr/share/icons/ubuntu-mono-dark/status/22/audio-volume-medium-panel.svg",
   ["audio-volume-low"] =
      "/usr/share/icons/ubuntu-mono-dark/status/22/audio-volume-low-panel.svg",
   ["audio-volume-muted"] =
      "/usr/share/icons/ubuntu-mono-dark/status/22/audio-volume-muted-panel.svg"
}

-- End of configuration

local awful = require("awful")
local wibox = require("wibox")
local pulseaudio = require("apw.pulseaudio")
local math = require("math")

local imagebox = wibox.widget.imagebox()
local pulseWidget = imagebox

local function update_volume()
   local icon
   if pulseaudio.Mute or pulseaudio.Volume <= 0.0 then
      icon = 'audio-volume-muted'
   elseif pulseaudio.Volume <= 0.5 then
      icon = "audio-volume-low"
   elseif pulseaudio.Volume <= 0.8 then
      icon ="audio-volume-medium"
   else
      icon = "audio-volume-high"
   end
   imagebox:set_image(theme_icons[icon])
end

function pulseWidget.SetMixer(command)
   mixer = command
end

function pulseWidget.Update()
   pulseaudio.UpdateState()
end

function pulseWidget.LaunchMixer()
   awful.spawn( mixer )
end

-- register mouse button actions
pulseWidget:buttons(
   awful.util.table.join(
      awful.button({ }, 1, pulseaudio.VolumeToggleMute),
      awful.button({ }, 3, pulseWidget.LaunchMixer),
      awful.button({ }, 4, pulseaudio.VolumeUp),
      awful.button({ }, 5, pulseaudio.VolumeDown)
   )
)

pulseaudio.register_callback(update_volume)
pulseaudio.start_timer()

-- awful.tooltip({
--     objects = { imagebox },
--     timer_function = function()
--        return string.format("Volume %d", pulseaudio.Volume * 100)
--     end,
-- })

return pulseWidget
