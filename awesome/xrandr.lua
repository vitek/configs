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
                mode = nil,
                connected = false
            }
            for _, flag in ipairs(info) do
                if flag == "primary" then
                    output.primary = true
                elseif flag == "connected" then
                    output.connected = true
                elseif flag:match('^%d+x%d+[+-]%d+[+-]%d+$') then
                    print('mode', flag)
                    output.mode = flag
                end
            end
            table.insert(outputs, output)
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

local function build_output_menu(output, outputs)
    local title = string.format(
        "%s (%s)", output.name, output.mode or "none")
    if output.primary then
        title = title .. ', primary'
    end

    local submenu = {}
    if output.connected then
        for _, pivot in ipairs(outputs) do
            if pivot.name ~= output.name and pivot.connected then
                table.insert(submenu, {
                    pivot.name, relation_menu(output, pivot)
                })
            end
        end

        table.insert(submenu, {
            "Make primray",
            string.format("xrandr --output %s --primary", output.name)
        })
    end
    table.insert(submenu, {
        "Switch Off",
        string.format("xrandr --output %s --off", output.name)
    })
    return {title, submenu}
end

local function xrandr_menu()
    local items = {}
    local outputs = list_outputs()
    for _, output in ipairs(outputs) do
        if output.connected or output.mode then
            table.insert(items, build_output_menu(output, outputs))
        end
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
