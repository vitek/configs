-- Standard awesome library
local awful = require("awful")
awful.rules = require("awful.rules")
require("awful.autofocus")
require("awful.dbus")
local gears = require("gears")
-- require("awful.startup_notification")
-- Widget and layout library
local wibox = require("wibox")
-- Theme handling library
local beautiful = require("beautiful")
-- Notification library
local naughty = require("naughty")
local menubar = require("menubar")

local vicious = require("vicious")

-- Delightful widgets
require('delightful.widgets.battery')
require('delightful.widgets.cpu')
--require('delightful.widgets.datetime')
--require('delightful.widgets.imap')
require('delightful.widgets.memory')
require('delightful.widgets.network')
require('delightful.widgets.pulseaudio')
--require('delightful.widgets.weather')


-- Load Debian menu entries
require("debian.menu")

local home_path = os.getenv("HOME")
local config_path = home_path .. "/.config/awesome"


function lock_screen()
   awful.util.spawn("gnome-screensaver-command --lock")
end


-- {{{ Variable definitions
-- Themes define colours, icons, and wallpapers
beautiful.init("/usr/share/awesome/themes/default/theme.lua")

-- This is used later as the default terminal and editor to run.
terminal = "x-terminal-emulator"
editor = "emacs"
editor_cmd = "emacs"
browser = "chromium-browser"
suspend_cmd = 'systemctl suspend -i'

-- Keyboard layout switch
kbd_dbus_next_cmd = "dbus-send --dest=ru.gentoo.KbddService /ru/gentoo/KbddService ru.gentoo.kbdd.next_layout"
kbd_dbus_first_cmd = "dbus-send --dest=ru.gentoo.KbddService /ru/gentoo/KbddService ru.gentoo.kbdd.set_layout uint32:0"

kbdstrings = {[0] = " en ", [1] = " ru "}


-- disable startup-notification globally
local oldspawn = awful.util.spawn
awful.util.spawn = function (s)
  oldspawn(s, false)
end

-- start apps
-- taken from https://bbs.archlinux.org/viewtopic.php?id=161403
awful.util.spawn("compton -C -b  -G " ..
                 "--paint-on-overlay --unredir-if-possible " ..
                 "--backend glx --glx-no-stencil --glx-no-rebind-pixmap")

awful.util.spawn(config_path .. "/scripts/keyboard.sh")
awful.util.spawn(config_path .. "/scripts/touchpad-init.sh")

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
modkey = "Mod4"

-- Table of layouts to cover with awful.layout.inc, order matters.
layouts =
{
    awful.layout.suit.tile,
    awful.layout.suit.tile.left,
    awful.layout.suit.tile.bottom,
    awful.layout.suit.tile.top,
    awful.layout.suit.floating,
--    awful.layout.suit.fair,
--    awful.layout.suit.fair.horizontal,
--    awful.layout.suit.spiral,
--    awful.layout.suit.spiral.dwindle,
--    awful.layout.suit.max,
--    awful.layout.suit.max.fullscreen,
--    awful.layout.suit.magnifier
}
-- }}}

-- {{{ Wallpaper
if beautiful.wallpaper then
    for s = 1, screen.count() do
        gears.wallpaper.maximized(beautiful.wallpaper, s, true)
    end
end
-- }}}

-- {{{ Tags
-- Define a tag table which hold all screen tags.
tags = {}
for s = 1, screen.count() do
    -- Each screen has its own tag table.
    tags[s] = awful.tag({ 1, 2, 3, 4, 5, 6, 7, 8, 9, 0 }, s, layouts[1])
end
-- }}}

-- {{{ Menu
-- Create a laucher widget and a main menu
myawesomemenu = {
   { "manual", terminal .. " -e man awesome" },
   { "edit config", editor_cmd .. " " .. awesome.conffile },
   { "restart", awesome.restart },
   { "quit", awesome.quit }
}

mymainmenu = awful.menu({ items = { { "Awesome", myawesomemenu, beautiful.awesome_icon },
                                    { "Debian", debian.menu.Debian_menu.Debian },
                                    { "Terminal", terminal },
                                    { "Editor", editor_cmd },
                                    { "Browser", browser },
                                    { "" },
                                    { "Lock", lock_screen },
                                    { "Sleep", suspend_cmd},
                                  }
                        })

mylauncher = awful.widget.launcher({ image = beautiful.awesome_icon,
                                     menu = mymainmenu })

-- Menubar configuration
menubar.utils.terminal = terminal -- Set the terminal for applications that require it
-- }}}

-- {{{ Wibox

-- Which widgets to install?
-- This is the order the widgets appear in the wibox.
delightful_widgets = {
    --delightful.widgets.network,
    --delightful.widgets.cpu,
    --delightful.widgets.memory,
    --delightful.widgets.weather,
    delightful.widgets.battery,
    delightful.widgets.pulseaudio,
    --delightful.widgets.datetime,
}

-- Widget configuration
delightful_config = {
    [delightful.widgets.cpu] = {
        command = 'gnome-system-monitor',
    },
    [delightful.widgets.memory] = {
        command = 'gnome-system-monitor',
    },
    -- [delightful.widgets.weather] = {
    --     {
    --         city = 'Moscow',
    --         command = 'gnome-www-browser http://ilmatieteenlaitos.fi/saa/Helsinki',
    --     },
    -- },
    [delightful.widgets.pulseaudio] = {
        mixer_command = 'pavucontrol',
    },
}

-- -- Text CPU widget
-- cpuwidget = wibox.widget.textbox()
-- -- Register widget
-- vicious.register(cpuwidget, vicious.widgets.cpu, " cpu: $1% ")

-- Graph CPU widget
cpuwidget = awful.widget.graph()
cpuwidget:set_width(50)
cpuwidget:set_background_color("#494B4F")
cpuwidget:set_color({ type = "linear", from = { 0, 0 }, to = { 10,0 },
                      stops = { {0, "#FF5656"}, {0.5, "#88A175"},
                         {1, "#AECF96" }}})
vicious.register(cpuwidget, vicious.widgets.cpu, "$1")

-- Graph Memory widget
-- Initialize widget
memwidget = awful.widget.progressbar()
-- Progressbar properties
memwidget:set_width(8)
memwidget:set_height(10)
memwidget:set_vertical(true)
memwidget:set_background_color("#494B4F")
memwidget:set_border_color("#000000")
memwidget:set_color({ type = "linear", from = { 0, 0 }, to = { 10,0 },
                      stops = { {0, "#AECF96"}, {0.5, "#88A175"},
                         {1, "#FF5656"}}})
vicious.register(memwidget, vicious.widgets.mem, "$1", 13)


-- Create a textclock widget
mytextclock = awful.widget.textclock(" %a %b %d, %H:%M:%S ", 1)

-- Keyboard layout widget
kbdwidget = wibox.widget.textbox(kbdstrings[1])
kbdwidget.border_width = 1
kbdwidget.border_color = beautiful.fg_normal
kbdwidget:set_text(" en ")

-- Create a wibox for each screen and add it
mywibox = {}
mypromptbox = {}
mylayoutbox = {}
mytaglist = {}
mytaglist.buttons = awful.util.table.join(
   awful.button({ }, 1, awful.tag.viewonly),
   awful.button({ modkey }, 1, awful.client.movetotag),
   awful.button({ }, 3, awful.tag.viewtoggle),
   awful.button({ modkey }, 3, awful.client.toggletag),
   awful.button({ }, 4, function(t)
         local screen = awful.tag.getscreen(t)
         if awful.tag.selected(screen) ~= tags[screen][1] then
            awful.tag.viewprev()
         end
   end),
   awful.button({ }, 5, function(t)
         local screen = awful.tag.getscreen(t)
         if awful.tag.selected(screen) ~= tags[screen][#tags[screen]] then
            awful.tag.viewnext(awful.tag.getscreen(t))
         end
   end)
)
mytasklist = {}
mytasklist.buttons = awful.util.table.join(
                     awful.button({ }, 1, function (c)
                                              if c == client.focus then
                                                  c.minimized = true
                                              else
                                                  -- Without this, the following
                                                  -- :isvisible() makes no sense
                                                  c.minimized = false
                                                  if not c:isvisible() then
                                                      awful.tag.viewonly(c:tags()[1])
                                                  end
                                                  -- This will also un-minimize
                                                  -- the client, if needed
                                                  client.focus = c
                                                  c:raise()
                                              end
                                          end),
                     awful.button({ }, 3, function ()
                                              if instance then
                                                  instance:hide()
                                                  instance = nil
                                              else
                                                  instance = awful.menu.clients({
                                                      theme = { width = 250 }
                                                  })
                                              end
                                          end),
                     awful.button({ }, 4, function ()
                                              awful.client.focus.byidx(1)
                                              if client.focus then client.focus:raise() end
                                          end),
                     awful.button({ }, 5, function ()
                                              awful.client.focus.byidx(-1)
                                              if client.focus then client.focus:raise() end
                                          end))

--for s = 1, screen.count() do
for s = 1, 1 do -- I want panel on first screen only
    -- Create a promptbox for each screen
    mypromptbox[s] = awful.widget.prompt()
    -- Create an imagebox widget which will contains an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    mylayoutbox[s] = awful.widget.layoutbox(s)
    mylayoutbox[s]:buttons(awful.util.table.join(
                           awful.button({ }, 1, function () awful.layout.inc(layouts, 1) end),
                           awful.button({ }, 3, function () awful.layout.inc(layouts, -1) end),
                           awful.button({ }, 4, function () awful.layout.inc(layouts, 1) end),
                           awful.button({ }, 5, function () awful.layout.inc(layouts, -1) end)))
    -- Create a taglist widget
    mytaglist[s] = awful.widget.taglist(s, awful.widget.taglist.filter.all, mytaglist.buttons)

    -- Create a tasklist widget
    mytasklist[s] = awful.widget.tasklist(s, awful.widget.tasklist.filter.currenttags, mytasklist.buttons)

    -- Create the wibox
    mywibox[s] = awful.wibox({ position = "top", screen = s })

    -- Widgets that are aligned to the left
    local left_layout = wibox.layout.fixed.horizontal()
    left_layout:add(mylauncher)
    left_layout:add(mytaglist[s])
    left_layout:add(cpuwidget)
    left_layout:add(memwidget)
    left_layout:add(mypromptbox[s])

    -- Widgets that are aligned to the right
    local right_layout = wibox.layout.fixed.horizontal()
    if s == 1 then
       right_layout:add(wibox.widget.systray())
       delightful.utils.fill_wibox_container(
          delightful_widgets, delightful_config, right_layout)
    end

    right_layout:add(kbdwidget)
    right_layout:add(mytextclock)
    -- right_layout:add(mylayoutbox[s])

    -- Now bring it all together (with the tasklist in the middle)
    local layout = wibox.layout.align.horizontal()
    layout:set_left(left_layout)
    --layout:set_middle(mytasklist[s])
    layout:set_right(right_layout)

    mywibox[s]:set_widget(layout)
end
-- }}}

-- {{{ Mouse bindings
root.buttons(awful.util.table.join(
                --     awful.button({ }, 4, awful.tag.viewnext),
                --     awful.button({ }, 5, awful.tag.viewprev)
))
-- }}}

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
    awful.key({ "Mod1" }, "F1", function () mymainmenu:toggle() end),
    --awful.key({ "Mod1" }, "F2", function () awful.util.spawn("/usr/bin/gnome-panel-control --run-dialog") end),

    -- Prompt
    awful.key({ modkey },            "r",     function () mypromptbox[1]:run() end),
    awful.key({ "Mod1" },            "F2",    function () mypromptbox[1]:run() end),

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
client.connect_signal("manage", function (c, startup)
    -- Enable sloppy focus
    c:connect_signal("mouse::enter", function(c)
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

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
-- }}}

-- dbus.add_match("system", "interface='org.freedesktop.login1.Manager',member='PrepareForSleep'")
-- dbus.connect_signal("org.freedesktop.login1.Manager",
--                 function(...)
--                    local data = {...}
--                    if data[1].member == "PrepareForSleep" then
--                       if data[2] then
--                          lock_screen()
--                          print("GOING TO SLEEP")
--                       end
--                    end
--                 end)

-- Force switch to english on screensaver activation/deactivation
dbus.add_match("session", "interface='org.gnome.ScreenSaver'")
dbus.connect_signal("org.gnome.ScreenSaver",
                function(...)
                   local data = {...}
                   if data[1].member == "ActiveChanged" then
                      os.execute(kbd_dbus_first_cmd)
                   end
                end)

-- Update keyboard widget state
dbus.request_name("session", "ru.gentoo.kbdd")
dbus.add_match("session", "interface='ru.gentoo.kbdd',member='layoutChanged'")
dbus.connect_signal("ru.gentoo.kbdd", function(...)
    local data = {...}
    local layout = data[2]
    kbdwidget:set_markup(kbdstrings[layout])
    end
)
