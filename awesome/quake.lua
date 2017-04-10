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
local all_consoles = {}

local function clamp(value, minValue, maxValue)
    return math.min(math.max(value, minValue), maxValue)
end

capi.client.connect_signal("manage", function(c)
    -- try to attach terminal from previous awesome run
    for _, console in pairs(all_consoles) do
        if (c.class == console.class and console.pid == nil and
                consoles[c.pid] == nil) then
            console.pid = c.pid
            consoles[console.pid] = console
            c.hidden = true
            return
        end
    end

    if consoles[c.pid] ~= nil then
        consoles[c.pid]:show(c)
    end
end)
capi.client.connect_signal("unmanage", function(c)
    if consoles[c.pid] ~= nil then
        consoles[c.pid]:hide(c)
    end
end)
capi.client.connect_signal("property::geometry", function(c)
    -- don't allow client to adjust its own size
    if consoles[c.pid] ~= nil then
        consoles[c.pid]:updateGeometry(c)
    end
end)

function QuakeConsole:findClient()
    if not self.pid then return end

    for c in awful.client.iterate(function (c)
        return c.pid == self.pid and c.class == self.class
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
        if self.pid then consoles[self.pid] = nil end
        local command =
            self.terminal .. " " .. string.format(self.argclass, self.class)
        self.pid = awful.spawn(command, false)
        -- Race condition much?
        consoles[self.pid] = self
        return -- Wait for client to spawn.
    end

    client.floating = true
    client.size_hints_honor = false

    -- Sticky and on top
    client.ontop = true
    client.above = true
    client.skip_taskbar = true
    client.sticky = true

    self:updateGeometry(client)

    -- This is not a normal window, don't apply any specific keyboard stuff
    client:buttons({})
    client:keys(self.keys)

    client.hidden = false
    client:raise()
    self.visible = true

    capi.client.focus = client
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

    -- The application to be invoked is:
    --   config.terminal .. " " .. string.format(config.argclass, config.class)
    config.terminal = config.terminal or "xterm" -- application to spawn
    config.class    = config.class or "QuakeConsoleNeedsUniqueName" -- window class
    config.argclass = config.argclass or "-class %s"     -- how to specify window name

    -- If width or height <= 1 this is a proportion of the workspace
    config.height   = config.height   or 0.25           -- height
    config.width    = config.width    or 1          -- width
    config.vert     = config.vert     or "top"          -- top, bottom or center
    config.horiz    = config.horiz    or "center"       -- left, right or center

    config.minHeight = config.minHeight or config.height
    config.maxHeight = config.maxHeight or 1.0

    config.screen   = config.screen
    config.visible  = config.visible or false -- Initially, not visible
    config.keys     = config.keys or {}

    local console = setmetatable(config, { __index = QuakeConsole })
    table.insert(all_consoles, console)
    return console
end

-- Toggle the console
function QuakeConsole:toggle()
    if self.visible then self:hide() else self:show() end
end

setmetatable(QuakeConsole, { __call = function(_, ...) return QuakeConsole:new(...) end })

return QuakeConsole
