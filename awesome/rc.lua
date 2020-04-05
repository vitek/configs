-- If LuaRocks is installed, make sure that packages installed through it are
-- found (e.g. lgi). If LuaRocks is not installed, do nothing.
pcall(require, "luarocks.loader")

-- @DOC_REQUIRE_SECTION@
-- Standard awesome library
local gears = require("gears")
local gfs = require("gears.filesystem")
local awful = require("awful")
require("awful.autofocus")
-- Widget and layout library
local wibox = require("wibox")
-- Theme handling library
local beautiful = require("beautiful")
-- Notification library, require core to disable dbus interface
local naughty = require("naughty.core")
--local menubar = require("menubar")
local hotkeys_popup = require("awful.hotkeys_popup").widget

local keyboard_layout = require("keyboard_layout")
local utils = require("utils")
local mywibar = require("mywibar")

local pulseaudio = require('apw.pulseaudio')
local brightness = require("brightness")
local quake = require("quake")
local xrandr = require("xrandr")

-- {{{ Error handling
-- @DOC_ERROR_HANDLING@
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
    naughty.notify({ preset = naughty.config.presets.critical,
                     title = "Oops, there were errors during startup!",
                     text = awesome.startup_errors })
end

-- Handle runtime errors after startup
do
    local in_error = false
    awesome.connect_signal("debug::error", function (err)
        -- Make sure we don't go into an endless error loop
        if in_error then return end
        in_error = true

        naughty.notify({ preset = naughty.config.presets.critical,
                         title = "Oops, an error happened!",
                         text = tostring(err) })
        in_error = false
    end)
end
-- }}}

-- {{{ Variable definitions
-- @LOCAL_SETTINGS@
local function load_settings(module_name, default_settings)
   local status, value = pcall(require, module_name)
   if not status then
      print('using default settings: ' .. value)
      return default_settings
   end
   setmetatable(value, {__index = default_settings})
   return value
end

local settings = load_settings(
   "local_settings", {
      enable_wibar = false,
      terminal = "gnome-terminal",
      editor = os.getenv("EDITOR") or "vim"
})

local function run_startup_script()
   awful.spawn(gfs.get_configuration_dir() .. 'scripts/start.sh')
end

run_startup_script()
-- @DOC_LOAD_THEME@
-- Themes define colours, icons, font and wallpapers.
beautiful.init(gears.filesystem.get_themes_dir() .. "default/theme.lua")

beautiful.tooltip_border_color = "#ff0000"
beautiful.tooltip_bg = "#aaaaaa"
beautiful.tooltip_fg = "#111111"
beautiful.tooltip_bg = "#000000"
beautiful.tooltip_fg = "#ffffff"
beautiful.tooltip_border_width = 0
beautiful.tooltip_align = "right"

-- @DOC_DEFAULT_APPLICATIONS@
-- This is used later as the default terminal and editor to run.
terminal = settings.terminal
editor = settings.editor
editor_cmd = terminal .. " -e " .. editor

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
modkey = "Mod4"

-- @DOC_LAYOUT@
-- Table of layouts to cover with awful.layout.inc, order matters.
awful.layout.layouts = {
    awful.layout.suit.tile,
    awful.layout.suit.tile.left,
    awful.layout.suit.tile.bottom,
    awful.layout.suit.tile.top,
    awful.layout.suit.floating,

    -- awful.layout.suit.fair,
    -- awful.layout.suit.fair.horizontal,
    -- awful.layout.suit.spiral,
    -- awful.layout.suit.spiral.dwindle,
    -- awful.layout.suit.max,
    -- awful.layout.suit.max.fullscreen,
    -- awful.layout.suit.magnifier,
    -- awful.layout.suit.corner.nw,
    -- awful.layout.suit.corner.ne,
    -- awful.layout.suit.corner.sw,
    -- awful.layout.suit.corner.se,
}
-- }}}

-- {{{ Menu
-- @DOC_MENU@
-- Create a launcher widget and a main menu
myawesomemenu = {
   { "hotkeys", function() return false, hotkeys_popup.show_help end},
   --{ "manual", terminal .. " -e man awesome" },
   --{ "edit config", editor_cmd .. " " .. awesome.conffile },
   --{ "restart", awesome.restart },
   --{ "quit", function() awesome.quit() end
   { "lock screen",  utils.lock_screen },
   { "quit session",  utils.quit_session }
}

mymainmenu = awful.menu({
      items = {
         { "awesome", myawesomemenu, beautiful.awesome_icon },
         { "open terminal", terminal },
         { "" },
         { "sleep", utils.suspend }
      }
})

mylauncher = awful.widget.launcher({
      image = "/usr/share/icons/ubuntu-mono-dark/status/22/system-devices-panel.svg",
      menu = mymainmenu
})

quakeconsole = quake{
    height = 0.3,
    keys = awful.util.table.join(
        awful.key({ modkey }, "Down", function (c)
            quakeconsole:incHeight(c, 0.1)
        end),
        awful.key({ modkey }, "Up", function (c)
            quakeconsole:incHeight(c, -0.1)
        end))
}

-- Menubar configuration
--menubar.utils.terminal = terminal -- Set the terminal for applications that require it
-- }}}

-- {{{ Wibar

local default_wallpaper = gfs.get_configuration_dir() .. 'background.jpeg'

-- @DOC_WALLPAPER@
local function set_wallpaper(s)
   local wallpaper
   -- Wallpaper
   if gfs.file_readable(default_wallpaper) then
      wallpaper = default_wallpaper
   elseif beautiful.wallpaper then
      wallpaper = beautiful.wallpaper
      -- If wallpaper is a function, call it with the screen
      if type(wallpaper) == "function" then
         wallpaper = wallpaper(s)
      end
   end
   if wallpaper then
      gears.wallpaper.maximized(wallpaper, s, true)
   end
end

-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
screen.connect_signal("property::geometry", set_wallpaper)

local function mypromptbox_run ()
   local mypromptbox =
      awful.screen.focused().mypromptbox or screen[1].mypromptbox
   if mypromptbox then
      mypromptbox:run()
   else
      awful.spawn("gnome-panel-control --run-dialog")
   end
end

-- @DOC_FOR_EACH_SCREEN@
awful.screen.connect_for_each_screen(function(s)
    -- Wallpaper
    set_wallpaper(s)

    -- Each screen has its own tag table.
    awful.tag({ "1", "2", "3", "4", "5", "6", "7", "8", "9" }, s,
        awful.layout.layouts[1])

    if settings.enable_wibar then
        -- only enable wibar on the main screen
        if s.index == 1 then
            mywibar.create_wibar(s)
        end
    end
end)
-- }}}

-- {{{ Mouse bindings
-- @DOC_ROOT_BUTTONS@
root.buttons(awful.util.table.join(
    awful.button({ }, 3, function () mymainmenu:toggle() end),
    awful.button({ modkey }, 4, utils.switch_tag_prev),
    awful.button({ modkey }, 5, utils.switch_tag_next)
))
-- }}}

-- {{{ Key bindings
-- @DOC_GLOBAL_KEYBINDINGS@
globalkeys = awful.util.table.join(
    awful.key({ modkey,           }, "s",      hotkeys_popup.show_help,
              {description="show help", group="awesome"}),
    awful.key({ modkey,           }, "Left",   awful.tag.viewprev,
              {description = "view previous", group = "tag"}),
    awful.key({ modkey,           }, "Right",  awful.tag.viewnext,
              {description = "view next", group = "tag"}),
    --awful.key({ modkey,           }, "Escape", awful.tag.history.restore,
    --          {description = "go back", group = "tag"}),

    awful.key({ modkey,           }, "j",
        function ()
            awful.client.focus.byidx( 1)
        end,
        {description = "focus next by index", group = "client"}
    ),
    awful.key({ modkey,           }, "k",
        function ()
            awful.client.focus.byidx(-1)
        end,
        {description = "focus previous by index", group = "client"}
    ),
    awful.key({ modkey,           }, "w", function () mymainmenu:show() end,
              {description = "show main menu", group = "awesome"}),

    -- Layout manipulation
    awful.key({ modkey, "Shift"   }, "j", function () awful.client.swap.byidx(  1)    end,
              {description = "swap with next client by index", group = "client"}),
    awful.key({ modkey, "Shift"   }, "k", function () awful.client.swap.byidx( -1)    end,
              {description = "swap with previous client by index", group = "client"}),
    awful.key({ modkey, "Control" }, "j", function () awful.screen.focus_relative( 1) end,
              {description = "focus the next screen", group = "screen"}),
    awful.key({ modkey, "Control" }, "k", function () awful.screen.focus_relative(-1) end,
              {description = "focus the previous screen", group = "screen"}),
    awful.key({ modkey,           }, "u", awful.client.urgent.jumpto,
              {description = "jump to urgent client", group = "client"}),
    awful.key({ modkey,           }, "Tab",
        function ()
            awful.client.focus.history.previous()
            if client.focus then
                client.focus:raise()
            end
        end,
        {description = "go back", group = "client"}),

    -- Standard program
    awful.key({ modkey,           }, "Return", function () awful.spawn(terminal) end,

              {description = "open a terminal", group = "launcher"}),
    awful.key({ modkey, "Control" }, "r", awesome.restart,
              {description = "reload awesome", group = "awesome"}),
    awful.key({ modkey, "Shift"   }, "q", awesome.quit,
              {description = "quit awesome", group = "awesome"}),

    awful.key({ modkey,           }, "l",     function () awful.tag.incmwfact( 0.05)          end,
              {description = "increase master width factor", group = "layout"}),
    awful.key({ modkey,           }, "h",     function () awful.tag.incmwfact(-0.05)          end,
              {description = "decrease master width factor", group = "layout"}),
    awful.key({ modkey, "Shift"   }, "h",     function () awful.tag.incnmaster( 1, nil, true) end,
              {description = "increase the number of master clients", group = "layout"}),
    awful.key({ modkey, "Shift"   }, "l",     function () awful.tag.incnmaster(-1, nil, true) end,
              {description = "decrease the number of master clients", group = "layout"}),
    awful.key({ modkey, "Control" }, "h",     function () awful.tag.incncol( 1, nil, true)    end,
              {description = "increase the number of columns", group = "layout"}),
    awful.key({ modkey, "Control" }, "l",     function () awful.tag.incncol(-1, nil, true)    end,
              {description = "decrease the number of columns", group = "layout"}),
    awful.key({ modkey,           }, "space", function () awful.layout.inc( 1)                end,
              {description = "select next", group = "layout"}),
    awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(-1)                end,
              {description = "select previous", group = "layout"}),

    awful.key({ modkey, "Control" }, "n",
              function ()
                  local c = awful.client.restore()
                  -- Focus restored client
                  if c then
                      client.focus = c
                      c:raise()
                  end
              end,
              {description = "restore minimized", group = "client"}),

    -- Prompt
    awful.key({ modkey },            "r",     mypromptbox_run,
              {description = "run prompt", group = "launcher"}),

    -- awful.key({ modkey }, "x",
    --           function ()
    --               awful.prompt.run {
    --                 prompt       = "Run Lua code: ",
    --                 textbox      = awful.screen.focused().mypromptbox.widget,
    --                 exe_callback = awful.util.eval,
    --                 history_path = awful.util.get_cache_dir() .. "/history_eval"
    --               }
    --           end,
    --           {description = "lua execute prompt", group = "awesome"}),
    -- Menubar
    --awful.key({ modkey }, "p", function() menubar.show() end,
    --{description = "show the menubar", group = "launcher"}),

    -- My custom keys
    awful.key({ "Mod1", "Control" }, "Left",   awful.tag.viewprev),
    awful.key({ "Mod1", "Control" }, "Right",  awful.tag.viewnext),
    awful.key({ "Mod1" }, "space",
       function ()
          print('switch')
          keyboard_layout.next(client.focus or {})
       end
    ),
    ---{description = "Language switch", group = "client"}),
    awful.key({ "Mod1",           }, "F1", function () mymainmenu:show() end,
       {description = "show main menu", group = "awesome"}),
    awful.key({ "Control",           }, "Escape", function () mymainmenu:show() end,
       {description = "show main menu", group = "awesome"}),
    awful.key({ "Mod1" },            "F2",
       mypromptbox_run,
       {description = "run prompt", group = "launcher"}),
    awful.key({ modkey },            "e",
       function () awful.spawn('emacs') end),
    awful.key({"Mod1", "Control"}, "l", utils.lock_screen),
    awful.key({"Mod1"}, "Escape", xrandr.menu),

    -- Volume control
    awful.key({ }, "XF86AudioRaiseVolume",  pulseaudio.VolumeUp),
    awful.key({ }, "XF86AudioLowerVolume",  pulseaudio.VolumeDown),
    awful.key({ }, "XF86AudioMute",         pulseaudio.VolumeToggleMute),
    awful.key({ }, "XF86MonBrightnessDown", brightness.dec),
    awful.key({ }, "XF86MonBrightnessUp", brightness.inc),

    awful.key({ modkey }, "F1", pulseaudio.VolumeToggleMute),
    awful.key({ modkey }, "F2", pulseaudio.VolumeDown),
    awful.key({ modkey }, "F3", pulseaudio.VolumeUp),
    awful.key({ modkey }, "F11", brightness.dec),
    awful.key({ modkey }, "F12", brightness.inc),

    -- Screenshots
    awful.key({ modkey }, "\\",
       function () awful.spawn("gnome-screenshot -wB") end),
    awful.key({ modkey, "Shift"}, "\\",
       function () awful.spawn("gnome-screenshot -wBc") end),
    awful.key({ modkey, "Mod1" }, "\\",
       function () awful.spawn("gnome-screenshot -a") end),
    awful.key({ modkey, "Mod1", "Shift"}, "\\",
       function () awful.spawn("gnome-screenshot -ac") end),

    -- quakeconsole
    awful.key({ modkey }, "`", function () quakeconsole:toggle() end),
    awful.key({ modkey }, "Escape", function () quakeconsole:toggle() end)
)

-- @DOC_CLIENT_KEYBINDINGS@
clientkeys = awful.util.table.join(
    awful.key({ modkey,           }, "f",
        function (c)
            c.fullscreen = not c.fullscreen
            c:raise()
        end,
        {description = "toggle fullscreen", group = "client"}),
    awful.key({ modkey, "Shift"   }, "c",      function (c) c:kill()                         end,
              {description = "close", group = "client"}),
    awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end,
              {description = "move to master", group = "client"}),
    awful.key({ modkey,           }, "o",      function (c) c:move_to_screen()               end,
              {description = "move to screen", group = "client"}),
    awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end,
              {description = "toggle keep on top", group = "client"}),
    awful.key({ modkey,           }, "n",
        function (c)
            -- The client currently has the input focus, so it cannot be
            -- minimized, since minimized clients can't have the focus.
            c.minimized = true
        end ,
        {description = "minimize", group = "client"}),
    awful.key({ modkey,           }, "m",
        function (c)
            c.maximized = not c.maximized
            c:raise()
        end ,
        {description = "maximize", group = "client"}),

    -- My custom keys
    awful.key({ "Mod1" }, "F4", function (c) c:kill() end)
)

-- @DOC_NUMBER_KEYBINDINGS@
-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it works on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
    globalkeys = awful.util.table.join(globalkeys,
        -- View tag only.
        awful.key({ modkey }, "#" .. i + 9,
                  function ()
                        local screen = awful.screen.focused()
                        local tag = screen.tags[i]
                        if tag then
                           tag:view_only()
                        end
                  end,
                  {description = "view tag #"..i, group = "tag"}),
        -- Toggle tag display.
        awful.key({ modkey, "Control" }, "#" .. i + 9,
                  function ()
                      local screen = awful.screen.focused()
                      local tag = screen.tags[i]
                      if tag then
                         awful.tag.viewtoggle(tag)
                      end
                  end,
                  {description = "toggle tag #" .. i, group = "tag"}),
        -- Move client to tag.
        awful.key({ modkey, "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = client.focus.screen.tags[i]
                          if tag then
                              client.focus:move_to_tag(tag)
                          end
                     end
                  end,
                  {description = "move focused client to tag #"..i, group = "tag"}),
        -- Toggle tag on focused client.
        awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = client.focus.screen.tags[i]
                          if tag then
                              client.focus:toggle_tag(tag)
                          end
                      end
                  end,
                  {description = "toggle focused client on tag #" .. i, group = "tag"})
    )
end

-- @DOC_CLIENT_BUTTONS@
clientbuttons = gears.table.join(
    awful.button({ }, 1, function (c)
        c:emit_signal("request::activate", "mouse_click", {raise = true})
    end),
    awful.button({ modkey }, 1, function (c)
        c:emit_signal("request::activate", "mouse_click", {raise = true})
        awful.mouse.client.move(c)
    end),
    awful.button({ modkey }, 3, function (c)
        c:emit_signal("request::activate", "mouse_click", {raise = true})
        awful.mouse.client.resize(c)
    end),

    awful.button({ modkey }, 4, function (c) utils.switch_tag_prev(c.screen) end),
    awful.button({ modkey }, 5, function (c) utils.switch_tag_next(c.screen) end)
)

-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Rules
-- Rules to apply to new clients (through the "manage" signal).
-- @DOC_RULES@
awful.rules.rules = {
    -- @DOC_GLOBAL_RULE@
    -- All clients will match this rule.
    { rule = { },
      properties = { border_width = beautiful.border_width,
                     border_color = beautiful.border_normal,
                     focus = awful.client.focus.filter,
                     raise = true,
                     keys = clientkeys,
                     buttons = clientbuttons,
                     screen = awful.screen.preferred,
                     placement = awful.placement.no_overlap+awful.placement.no_offscreen,
                     size_hints_honor = false,
                     maximized = false
     }
    },

    -- @DOC_FLOATING_RULE@
    -- Floating clients.
    { rule_any = {
        instance = {
          "DTA",  -- Firefox addon DownThemAll.
          "copyq",  -- Includes session name in class.
        },
        class = {
          "Arandr",
          "Gpick",
          "Kruler",
          "MessageWin",  -- kalarm.
          "Sxiv",
          "Wpa_gui",
          "pinentry",
          "veromix",
          "xtightvncviewer"},

        name = {
          "Event Tester",  -- xev.
        },
        role = {
          "AlarmWindow",  -- Thunderbird's calendar.
          "pop-up",       -- e.g. Google Chrome's (detached) Developer Tools.
        }
      }, properties = { floating = true }},

    -- @DOC_DIALOG_RULE@
    -- Add titlebars to normal clients and dialogs
    { rule_any = {type = { "normal", "dialog" }
      }, properties = { titlebars_enabled = false }
    },

    -- Emacs has its own keyboard switcher
    { rule_any = { class =  { "emacs", "Emacs" } },
      properties = { disable_kbd_switch = true } },

    { rule_any = { class =  { "chrome-browser",
                              "chromium-browser",
                              "yandex-browser-beta" } },
      properties = { floating = false, maximized = false } }

    -- Set Firefox to always map on the tag named "2" on screen 1.
    -- { rule = { class = "Firefox" },
    --   properties = { screen = 1, tag = "2" } },
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
-- @DOC_MANAGE_HOOK@
client.connect_signal("manage", function (c)
    -- Set the windows at the slave,
    -- i.e. put it at the end of others instead of setting it master.
    -- if not awesome.startup then awful.client.setslave(c) end

    if awesome.startup
      and not c.size_hints.user_position
      and not c.size_hints.program_position then
        -- Prevent clients from being unreachable after screen count changes.
        awful.placement.no_offscreen(c)
    end
end)

-- @DOC_TITLEBARS@
-- Add a titlebar if titlebars_enabled is set to true in the rules.
client.connect_signal("request::titlebars", function(c)
    -- buttons for the titlebar
    local buttons = awful.util.table.join(
        awful.button({ }, 1, function()
            client.focus = c
            c:raise()
            awful.mouse.client.move(c)
        end),
        awful.button({ }, 3, function()
            client.focus = c
            c:raise()
            awful.mouse.client.resize(c)
        end)
    )

    awful.titlebar(c) : setup {
        { -- Left
            awful.titlebar.widget.iconwidget(c),
            buttons = buttons,
            layout  = wibox.layout.fixed.horizontal
        },
        { -- Middle
            { -- Title
                align  = "center",
                widget = awful.titlebar.widget.titlewidget(c)
            },
            buttons = buttons,
            layout  = wibox.layout.flex.horizontal
        },
        { -- Right
            awful.titlebar.widget.floatingbutton (c),
            awful.titlebar.widget.maximizedbutton(c),
            awful.titlebar.widget.stickybutton   (c),
            awful.titlebar.widget.ontopbutton    (c),
            awful.titlebar.widget.closebutton    (c),
            layout = wibox.layout.fixed.horizontal()
        },
        layout = wibox.layout.align.horizontal
    }
end)

-- Enable sloppy focus, so that focus follows mouse.
client.connect_signal("mouse::enter", function(c)
    if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
        and awful.client.focus.filter(c) then
        client.focus = c
    end
end)

-- @DOC_BORDER@
client.connect_signal("focus", function(c)
    c.border_color = beautiful.border_focus
end)
client.connect_signal("unfocus", function(c)
    c.border_color = beautiful.border_normal
end)
-- }}}

-- On screen disconnect move client to the same tag number other screen
client.connect_signal("request::tag", function (c, t, hints)
    if hints and hints.reason == "screen-removed" then
        local fallback = nil
        for other_screen in screen do
            if #other_screen.tags > 0 then
                fallback = other_screen.tags[c.first_tag.index] or
                    other_screen.tags[1]
                break
            end
        end
        if fallback then
            c.screen_evicted = true
            c:move_to_tag(fallback)
        end
    end
end)

local screen_state = {
    selected_tag
}
-- Move client back to the screen and tag
-- TODO: restore full state
screen.connect_signal("added", function (s)
    for _, c in ipairs(client.get()) do
        if c.screen_evicted then
            local tag = s.tags[c.first_tag.index]
            if tag then
                c:move_to_tag(tag)
            end
            c.screen_evicted = nil
        end
    end

    local tag = s.tags[screen_state.selected_tag]
    if tag then
        tag:view_only()
    end
    screen_state.selected_tag = nil
end)

tag.connect_signal("removal-pending", function (t)
    if t.selected then
        screen_state.selected_tag = t.index
    end
end)
