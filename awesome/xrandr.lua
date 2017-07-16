-- TODO:
--  - monitor display attachmet/detachment with udevadm monitor
--  - store configuration changes
--  - restore configration when display attached

local awful = require("awful")

local function split(value)
    local result = {}
    for i in string.gmatch(value, "%S+") do
        table.insert(result, i)
    end
    return result
end

local function list_outputs()
    local xrandr = io.popen("xrandr -q --current")
    local outputs = {}

    for line in xrandr:lines() do
        local name, info, details = line:match(
            "^([%w-]+) (.*) (%(.+%).*)$")
        if name then
            info = split(info)
            local output = {
                name = name,
                primary = false,
                mode = info[#info],
                connected = false
            }
            for _, flag in ipairs(info) do
                if flag == "primary" then
                    output.primary = true
                elseif flag == "connected" then
                    output.connected = true
                end
            end
            if output.connected then
                table.insert(outputs, output)
            end
        end
    end
    xrandr:close()
    return outputs
end

local function xrandr_move(output, pivot, where)
    return string.format(
        "xrandr --output %s --auto --%s %s", output, where, pivot
    )
end

local function relation_menu(output, pivot)
    return {
        {
            string.format("Right-Of %s", pivot.name),
            xrandr_move(output.name, pivot.name, "right-of")
        },
        {
            string.format("Left-Of %s", pivot.name),
            xrandr_move(output.name, pivot.name, "left-of")
        },
        {
            string.format("Above %s", pivot.name),
            xrandr_move(output.name, pivot.name, "above")
        },
        {
            string.format("Bellow %s", pivot.name),
            xrandr_move(output.name, pivot.name, "below")
        },
        {
            string.format("Mirror %s", pivot.name),
            xrandr_move(output.name, pivot.name, "same-as")
        }
    }
end

local function xrandr_menu()
    local items = {}
    local outputs = list_outputs()
    for _, output in ipairs(outputs) do
        local title = string.format(
            "%s (%s)", output.name, output.mode or "unknown")
        if output.primary then
            title = title .. ', primary'
        end

        local submenu = {}
        for _, pivot in ipairs(outputs) do
            if pivot.name ~= output.name then
                table.insert(submenu, {
                    pivot.name, relation_menu(output, pivot)
                })
            end
        end

        table.insert(submenu, {
            "Make primray",
            string.format("xrandr --output %s --primary", output.name)
        })
        table.insert(submenu, {
            "Switch Off",
            string.format("xrandr --output %s --off", output.name)
        })
        table.insert(items, {title, submenu})
    end

    local xrandr_menu = awful.menu({
        items = items, theme = {
            width = 200
        }
    })
    xrandr_menu:show()
end

return {
    menu = xrandr_menu
}
