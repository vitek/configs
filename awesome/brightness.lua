local awful = require "awful"

local brightness = {}

local DEFAULT_BRIGHTNESS = 60
local BRIGHTNESS_PATH = os.getenv("HOME") .. '/.config/awesome/.brightness'
local STEP = 10

local function save_brightness(value)
    local fp = io.open(BRIGHTNESS_PATH, "w")
    if fp ~= nil then
        fp:write(tostring(value) .. '\n')
        fp:close()
    end
end

local function load_brightness()
    local fp = io.open(BRIGHTNESS_PATH, "r")
    if fp == nil then return DEFAULT_BRIGHTNESS end
    local value = tonumber(fp:read("*line"))
    fp:close()
    return value or DEFAULT_BRIGHTNESS
end

local function set_brightness(value)
    awful.spawn("xbacklight -set " .. tostring(value))
end

local busy = false

local function brightness_set_callback(stdout, stderr, reason, exit_code)
    awful.spawn.easy_async("xbacklight", function (
            stdout, stderr, reason, exit_code)
        local brightness = tonumber(stdout) or DEFAULT_BRIGHTNESS
        save_brightness(brightness)
        busy = false
    end)
end

function brightness.inc()
    if busy then return end
    busy = true
    awful.spawn.easy_async(
        string.format(
            "xbacklight -inc %d -steps %d", STEP, STEP),
        brightness_set_callback)
end
function brightness.dec()
    if busy then return end
    busy = true
    awful.spawn.easy_async(
        string.format(
            "xbacklight -dec %d -steps %d", STEP, STEP),
        brightness_set_callback)
end

-- restore original brightness
set_brightness(load_brightness())
return brightness
