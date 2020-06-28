-- Quake like console on top
--
-- Based on https://github.com/Stebalien/awesomewm-config/blob/master/quake.lua
--
-- Similar to:
--   http://git.sysphere.org/awesome-configs/tree/scratch/drop.lua

-- But uses a different implementation. The main difference is that we
-- are able to detect the Quake console from its name
-- (QuakeConsoleNeedsUniqueName by default).

-- Use:

-- local quake = require("quake")
-- local quakeconsole = {}
-- for s = 1, screen.count() do
--    quakeconsole[s] = quake({ terminal = config.terminal,
--                  height = 0.3,
--                              screen = s })
-- end

-- config.keys.global = awful.util.table.join(
--    config.keys.global,
--    awful.key({ modkey }, "`",
--       function () quakeconsole[mouse.screen]:toggle() end)

-- If you have a rule like "awful.client.setslave" for your terminals,
-- ensure you use an exception for
-- QuakeConsoleNeedsUniqueName. Otherwise, you may run into problems
-- with focus.

local setmetatable = setmetatable
local string = string
local awful  = require("awful")
local naughty = naughty
local math = require("math")
local os = { time = os.time }
local capi = {
    mouse = mouse,
    screen = screen,
    client = client,
    timer = timer
}

local QuakeConsole = {}
local consoles = {}

local next_console_id = 0
local function get_next_console_id()
    local id = next_console_id
    next_console_id = next_console_id + 1
    return id
end

local function clamp(value, minValue, maxValue)
    return math.min(math.max(value, minValue), maxValue)
end

awful.client.property.persist("quake_console_id", "number")
capi.client.connect_signal("manage", function(c)
    local console = consoles[c.quake_console_id]
    if console ~= nil then
        if console.waitingForAppear then
            console:show(c)
        else
            -- console from prevous awesome run
            c.hidden = true
        end
    end
end)
capi.client.connect_signal("unmanage", function(c)
    local console = consoles[c.quake_console_id]
    if console ~= nil then
        console:hide(c)
    end
end)

function QuakeConsole:findClient()
    for c in awful.client.iterate(function (c)
        return c.quake_console_id == self.console_id
    end) do return c end
end

function QuakeConsole:hide(client)
    if not self.visible then return end

    if not client then client = self:findClient() end
    if client then client.hidden = true end
    self.visible = false
end

-- Display
function QuakeConsole:show(client)
    if self.visible then return end

    -- First, we locate the terminal
    if not client then client = self:findClient() end

    if not client then
        if not self.waitingForAppear then
            awful.spawn(self.terminal, {
                quake_console_id = self.console_id
            })
            self.waitingForAppear = true
        end
        return
    end

    client.floating = true
    client.size_hints_honor = false

    -- Sticky and on top
    client.ontop = true
    client.above = true
    client.skip_taskbar = true
    client.sticky = true

    self:updateGeometry(client)

    client:connect_signal("request::geometry", function(c, context, geo)
        if context ~= 'mouse.resize' then return end
        local screen = self.screen or capi.mouse.screen
        local geom = capi.screen[screen].workarea
        self.height = (geo.height + 2 * client.border_width) / geom.height
    end)
    client:connect_signal("property::geometry", function(c)
        -- don't allow client to adjust its own size
        self:updateGeometry(c)
    end)

    -- This is not a normal window, don't apply any specific keyboard stuff
    client:buttons(self.buttons)
    client:keys(self.keys)

    client.hidden = false
    client:raise()
    capi.client.focus = client

    self.visible = true
    self.waitingForAppear = false
end

function QuakeConsole:updateGeometry(client)
    -- Comptute size
    local screen = self.screen or capi.mouse.screen
    local geom = capi.screen[screen].workarea
    local width, height = self.width, self.height
    if width  <= 1 then width = geom.width * width end
    if height <= 1 then height = geom.height * height end
    local x, y
    if     self.horiz == "left"  then x = geom.x
    elseif self.horiz == "right" then x = geom.width + geom.x - width
    else   x = geom.x + (geom.width - width)/2 end
    if     self.vert == "top"    then y = geom.y
    elseif self.vert == "bottom" then y = geom.height + geom.y - height
    else   y = geom.y + (geom.height - height)/2 end

    width = width - 2 * client.border_width
    height = height - 2 * client.border_width

    -- Resize
    client:geometry({ x = x, y = y, width = width, height = height })
end

function QuakeConsole:incHeight(client, inc)
    inc = inc or 0.1
    self.height = clamp(self.height + inc, self.minHeight, self.maxHeight)
    self:updateGeometry(client)
end

-- Create a console
function QuakeConsole:new(config)
    -- The "console" object is just its configuration.

    config.console_id = config.console_id or get_next_console_id()
    config.terminal = config.terminal or terminal -- application to spawn

    -- If width or height <= 1 this is a proportion of the workspace
    config.height   = config.height   or 0.25           -- height
    config.width    = config.width    or 1          -- width
    config.vert     = config.vert     or "top"          -- top, bottom or center
    config.horiz    = config.horiz    or "center"       -- left, right or center

    config.minHeight = config.minHeight or config.height
    config.maxHeight = config.maxHeight or 1.0

    config.screen   = config.screen
    config.visible  = config.visible or false -- Initially, not visible
    config.buttons  = config.buttons or {}
    config.keys     = config.keys or {}

    local console = setmetatable(config, { __index = QuakeConsole })
    consoles[config.console_id] = console
    return console
end

-- Toggle the console
function QuakeConsole:toggle()
    if self.visible then self:hide() else self:show() end
end

setmetatable(QuakeConsole, {
    __call = function(_, ...)
        return QuakeConsole:new(...)
    end
})

return QuakeConsole
