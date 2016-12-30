local capi = {awesome = awesome}

local keyboard_layout = {}

local last_xcb_layout_group = 0

local function set_layout(c, group)
   if group == nil then
      group = c.xcb_layout_group or last_xcb_layout_group
   end
   --print('set_layout: ' .. tostring(c) .. " group: " .. tostring(group))
   awesome.xkb_set_layout_group(group)
   last_xcb_layout_group = awesome.xkb_get_layout_group()
   c.xcb_layout_group = last_xcb_layout_group
end

function keyboard_layout.cycle(c, inc)
   local current = awesome.xkb_get_layout_group()
   set_layout(c, current + inc)
end
function keyboard_layout.next(c)
   keyboard_layout.cycle(c, 1)
end
function keyboard_layout.prev(c)
   keyboard_layout.cycle(c, -1)
end

client.connect_signal("focus", function(c) set_layout(c, nil) end)
capi.awesome.connect_signal("xkb::group_changed",
                            function ()
                               keyboard_layout.cycle(client.focus or {}, 0)
                            end
)

return keyboard_layout
