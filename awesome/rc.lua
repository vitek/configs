-- Standard awesome library
require("awful")
require("awful.autofocus")
require("awful.rules")
require("awful.dbus")
-- require("awful.startup_notification")
-- Theme handling library
require("beautiful")
-- Notification library
--require("naughty")

local home_path = os.getenv("HOME")
local config_path = home_path .. "/.config/awesome"
local locker_path = config_path .. '/locker.sh'


function lock_screen()
   awful.util.spawn(locker_path .. " lock")
end


-- {{{ Variable definitions
-- Themes define colours, icons, and wallpapers
beautiful.init("/usr/share/awesome/themes/default/theme.lua")

-- This is used later as the default terminal and editor to run.
terminal = "x-terminal-emulator"
editor = 'emacs'
editor_cmd = 'emacs'
browser = "chromium-browser"


-- disable startup-notification globally
local oldspawn = awful.util.spawn
awful.util.spawn = function (s)
  oldspawn(s, false)
end

-- start apps
awful.util.spawn("/usr/bin/gnome-panel")
awful.util.spawn("/usr/bin/xcompmgr")
awful.util.spawn("/usr/bin/kbdd")
awful.util.spawn("setxkbmap -layout \"us,ru\"")
awful.util.spawn(locker_path .. " autolocker")
awful.util.spawn(config_path .. "/touchpad-init.sh")

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
modkey = "Mod4"

-- Table of layouts to cover with awful.layout.inc, order matters.
layouts =
{
    awful.layout.suit.floating,
    awful.layout.suit.tile,
    awful.layout.suit.tile.left,
    awful.layout.suit.tile.bottom,
    awful.layout.suit.tile.top,
--    awful.layout.suit.fair,
--    awful.layout.suit.fair.horizontal,
--    awful.layout.suit.spiral,
--    awful.layout.suit.spiral.dwindle,
--    awful.layout.suit.max,
--    awful.layout.suit.max.fullscreen,
--    awful.layout.suit.magnifier
}
-- }}}

-- {{{ Tags
-- Define a tag table which hold all screen tags.
tags = {}
for s = 1, screen.count() do
    -- Each screen has its own tag table.
    tags[s] = awful.tag({ 1, 2, 3, 4, 5, 6, 7, 8, 9, 0 }, s, layouts[1])
end
-- }}}

-- {{{ Mouse bindings
-- root.buttons(awful.util.table.join(
--     awful.button({ }, 4, awful.tag.viewnext),
--     awful.button({ }, 5, awful.tag.viewprev)
-- ))
-- }}}


function changeview(delta)
   for idx = 1, screen.count() do
      awful.tag.viewidx(delta, screen[idx])
   end
end


-- Keyboard layout switch
kbd_dbus_next_cmd = "dbus-send --dest=ru.gentoo.KbddService /ru/gentoo/KbddService ru.gentoo.kbdd.next_layout"


-- {{{ Key bindings
globalkeys = awful.util.table.join(
   awful.key({ modkey,           }, "Left",   awful.tag.viewprev),
   awful.key({ modkey,           }, "Right",  awful.tag.viewnext),
   awful.key({ modkey,           }, "Escape", awful.tag.history.restore),

    awful.key({ modkey,           }, "j",
        function ()
            awful.client.focus.byidx( 1)
            if client.focus then client.focus:raise() end
        end),
    awful.key({ modkey,           }, "k",
        function ()
            awful.client.focus.byidx(-1)
            if client.focus then client.focus:raise() end
        end),

    -- Layout manipulation
    awful.key({ modkey, "Shift"   }, "j", function () awful.client.swap.byidx(  1)    end),
    awful.key({ modkey, "Shift"   }, "k", function () awful.client.swap.byidx( -1)    end),
    awful.key({ modkey, "Control" }, "j", function () awful.screen.focus_relative( 1) end),
    awful.key({ modkey, "Control" }, "k", function () awful.screen.focus_relative(-1) end),
    awful.key({ modkey,           }, "u", awful.client.urgent.jumpto),
    awful.key({ modkey,           }, "Tab",
        function ()
            awful.client.focus.history.previous()
            if client.focus then
                client.focus:raise()
            end
        end),

    -- Standard program
    awful.key({ modkey,           }, "Return", function () awful.util.spawn(terminal) end),
    awful.key({ modkey, "Control" }, "r", awesome.restart),
    awful.key({ modkey, "Shift"   }, "q", awesome.quit),

    awful.key({ modkey,           }, "l",     function () awful.tag.incmwfact( 0.05)    end),
    awful.key({ modkey,           }, "h",     function () awful.tag.incmwfact(-0.05)    end),
    awful.key({ modkey, "Shift"   }, "h",     function () awful.tag.incnmaster( 1)      end),
    awful.key({ modkey, "Shift"   }, "l",     function () awful.tag.incnmaster(-1)      end),
    awful.key({ modkey, "Control" }, "h",     function () awful.tag.incncol( 1)         end),
    awful.key({ modkey, "Control" }, "l",     function () awful.tag.incncol(-1)         end),
    awful.key({ modkey,           }, "space", function () awful.layout.inc(layouts,  1) end),
    awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(layouts, -1) end),


    -- GNOME-alike bindings
    --- Workspace control
    awful.key({ "Mod1", "Control" }, "Left",   awful.tag.viewprev),
    awful.key({ "Mod1", "Control" }, "Right",  awful.tag.viewnext),

    --- Run program
    awful.key({ "Mod1" }, "F1", function () awful.util.spawn_with_shell("sleep .2; /usr/bin/gnome-panel-control --main-menu") end),
    awful.key({ "Mod1" }, "F2", function () awful.util.spawn("/usr/bin/gnome-panel-control --run-dialog") end),

    awful.key({ "Mod1", "Alt"     }, "Tab",
              function ()
                 awful.menu.menu_keys.down = { "Down", "Alt_L" }
                 local cmenu = awful.menu.clients({width=400}, {keygrabber=true, coords={x=0, y=10}})
              end),
    -- Lock screen
    awful.key({"Mod1", "Control"}, "l", lock_screen),
    -- Keyboard layout switch
    awful.key({"Alt", "Mod1"}, "space",
              function () os.execute(kbd_dbus_next_cmd) end)
)

clientkeys = awful.util.table.join(
    awful.key({ modkey,           }, "f",      function (c) c.fullscreen = not c.fullscreen  end),
    awful.key({ modkey, "Shift"   }, "c",      function (c) c:kill()                         end),
    awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle                     ),
    awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end),
    awful.key({ modkey,           }, "o",      awful.client.movetoscreen                        ),
    awful.key({ modkey, "Shift"   }, "r",      function (c) c:redraw()                       end),
    awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end),
--    awful.key({ modkey,           }, "n",      function (c) c.minimized = not c.minimized    end),

    awful.key({ "Mod1" }, "F10",
        function (c)
           local new_state = not c.maximized_horizontal
           c.maximized_horizontal = new_state
           c.maximized_vertical   = new_state
        end),
    awful.key({ "Mod1" }, "F4", function (c) c:kill() end)
)


-- Compute the maximum number of digit we need, limited to 9
keynumber = 0
for s = 1, screen.count() do
   keynumber = math.min(9, math.max(#tags[s], keynumber));
end

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it works on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, keynumber do
    globalkeys = awful.util.table.join(globalkeys,
        awful.key({ modkey }, "#" .. i + 9,
                  function ()
                        local screen = mouse.screen
                        if tags[screen][i] then
                            awful.tag.viewonly(tags[screen][i])
                        end
                  end),
        awful.key({ modkey, "Control" }, "#" .. i + 9,
                  function ()
                      local screen = mouse.screen
                      if tags[screen][i] then
                          awful.tag.viewtoggle(tags[screen][i])
                      end
                  end),
        awful.key({ modkey, "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus and tags[client.focus.screen][i] then
                          awful.client.movetotag(tags[client.focus.screen][i])
                      end
                  end),
        awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus and tags[client.focus.screen][i] then
                          awful.client.toggletag(tags[client.focus.screen][i])
                      end
                  end))
end

clientbuttons = awful.util.table.join(
    awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
    awful.button({ modkey }, 1, awful.mouse.client.move),
    awful.button({ modkey }, 3, awful.mouse.client.resize),

    awful.button({ "Mod1" }, 1, awful.mouse.client.move),
    awful.button({ "Mod1" }, 3, awful.mouse.client.resize))

-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Rules
awful.rules.rules = {
    -- All clients will match this rule.
    { rule = { },
      properties = { border_width = beautiful.border_width,
                     border_color = beautiful.border_normal,
                     focus = true,
                     keys = clientkeys,
                     buttons = clientbuttons,
                     size_hints_honor=false, } },
    { rule = { class = "asisp1126.exe" },
      properties = { floating = true } },
    { rule = { class = "MPlayer" },
      properties = { floating = true } },
    { rule = { class = "pinentry" },
      properties = { floating = true } },
    -- { rule = { class = "gimp" },
    -- properties = { floating = true }, callback = awful.titlebar.add},
    { rule = { class = "Git-gui" },
      properties = { floating = true } },

    -- Unity2d hints
    { rule = { class = "Unity-2d-panel"},
      properties =  { above=true, struts=true }},
    { rule = { class = "Unity-2d-launcher"},
      properties =  { above = true, sticky=true }},
    { rule = { class = "Unity-2d-places"},
      properties =  { above = true }},

    -- Set Firefox to always map on tags number 2 of screen 1.
    -- { rule = { class = "Firefox" },
    --   properties = { tag = tags[1][2] } },
}
-- }}}

-- -- Add or remove title bar
-- function update_titlebar(c)
--    local enable_titlebar = (
--       awful.layout.get(c.screen) == awful.layout.suit.floating or
--       awful.client.floating.get(c))

--    if c.fullscreen or c.maximized_vertical or c.maximized_horizontal then
--       enable_titlebar = false
--    end

--    if c.titlebar and not enable_titlebar then
--       awful.titlebar.remove(c)
--    elseif not c.titlebar and enable_titlebar then
--       awful.titlebar.add(c, { modkey = modkey })
--    end
-- end

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.add_signal("manage", function (c, startup)
    -- Add a titlebar
    -- awful.titlebar.add(c, { modkey = modkey })
    -- update_titlebar(c)
    -- c:add_signal("property::floating", update_titlebar)
    -- c:add_signal("property::maximized_vertical", update_titlebar)
    -- c:add_signal("property::maximized_horizontal", update_titlebar)

    -- Enable sloppy focus
    c:add_signal("mouse::enter", function(c)
        if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
            and awful.client.focus.filter(c) then
            client.focus = c
        end
    end)

    c.size_hints_honor = false

    if not startup then
        -- Set the windows at the slave,
        -- i.e. put it at the end of others instead of setting it master.
        -- awful.client.setslave(c)

        -- Put windows in a smart way, only if they does not set an initial position.
        if not c.size_hints.user_position and not c.size_hints.program_position then
            awful.placement.no_overlap(c)
            awful.placement.no_offscreen(c)
        end
    end
end)

client.add_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.add_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
-- }}}

-- for s = 1, screen.count() do
--    awful.tag.attached_add_signal(
--       s, "property::layout",
--       function (tag)
--          for _, c in pairs(tag:clients()) do
--             update_titlebar(c)
--          end
--       end)
-- end

dbus.request_name("system", "org.freedesktop.login1.Manager")
dbus.add_match("system", "interface='org.freedesktop.login1.Manager',member='PrepareForSleep'")
dbus.add_signal("org.freedesktop.login1.Manager",
                function(...)
                   local data = {...}
                   if data[1]['member'] == "PrepareForSleep" then
                      if data[2] then
                         lock_screen()
                         print("GOING TO SLEEP")
                      end
                   end
                end)
