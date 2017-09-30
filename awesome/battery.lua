local math = require("math")

local awful = require("awful")
local wibox = require("wibox")

local vicious = require("vicious")

local icondir_base = "/usr/share/icons/ubuntu-mono-dark/status/22/"
local battery_icons = {
    ["charged"] =
        icondir_base .. "battery-charged.svg",
    ["charging-000"] =
        icondir_base .. "battery-000-charging.svg",
    ["charging-020"] =
        icondir_base .. "battery-020-charging.svg",
    ["charging-040"] =
        icondir_base .. "battery-040-charging.svg",
    ["charging-060"] =
        icondir_base .. "battery-060-charging.svg",
    ["charging-080"] =
        icondir_base .. "battery-080-charging.svg",
    ["charging-100"] =
        icondir_base .. "battery-100-charging.svg",
    ["discharging-000"] =
        icondir_base .. "battery-000.svg",
    ["discharging-020"] =
        icondir_base .. "battery-020.svg",
    ["discharging-040"] =
        icondir_base .. "battery-040.svg",
    ["discharging-060"] =
        icondir_base .. "battery-060.svg",
    ["discharging-080"] =
        icondir_base .. "battery-080.svg",
    ["discharging-100"] =
        icondir_base .. "battery-100.svg"
}

local iconkey_fmt = {
    ["↯"] = "charged",
    ["+"] = "charging-%03d",
    ["-"] = "discharging-%03d"
}
local tooltip_fmt = {
    ["↯"] = "Full, %d%%, %s left",
    ["⌁"] = "Unknown state",
    ["+"] = "Charging %d%%, %s until charged",
    ["-"] = "Discharging %d%%, %s remaining"
}

local function round(value, div)
    local k = value / (div or 1.0)
    local f = math.floor(k)
    local c = math.ceil(k)

    if math.abs(k - f) < math.abs(k - c) then
        return f * div
    else
        return c * div
    end
end

local function BatteryWidget(opts)
    local options = {
        batname = opts.batname or "BAT0",
        icon_theme = opts.icon_theme or battery_icons,
        refresh = opts.refresh or 61
    }
    local batwidget = wibox.widget.imagebox()
    batwidget:set_image(battery_icons["charged"])
    function batwidget.set_markup(self, value)
        if value ~= nil then
            self.visible = true
            self:set_image(options.icon_theme[value])
        else
            self.visible = false
        end
    end
    vicious.cache(vicious.widgets.bat)
    vicious.register(batwidget, vicious.widgets.bat, function (widget, data)
        local state, percentage = unpack(data)
        local fmt = iconkey_fmt[state]
        if fmt ~= nil then
            return string.format(fmt, round(percentage, 20))
        end
    end, options.refresh, options.batname)
    awful.tooltip({
        objects = { batwidget },
        timer_function = function()
            local data = vicious.widgets.bat(nil, options.batname)
            local state, percentage, remaining = unpack(data)
            return string.format(tooltip_fmt[state], percentage, remaining)
        end,
    })
    return batwidget
end

return {
    BatteryWidget = BatteryWidget
}
