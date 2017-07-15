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

local function xrandr_menu()
    local secondary = {}
    local primary

    for _, output in ipairs(list_outputs()) do
        if output.primary then
            primary = output
        else
            table.insert(secondary, output)
        end
    end

    local items = {
        {
            string.format("%s (%s), primray",
                primary.name, primary.mode or "unknown")
        }
    }
    for _, output in ipairs(secondary) do
        table.insert(items, {
            string.format("%s (%s)", output.name, output.mode or "unknown"), {
                {
                    string.format("Right-Of %s", primary.name),
                    xrandr_move(output.name, primary.name, "right-of")
                },
                {
                    string.format("Left-Of %s", primary.name),
                    xrandr_move(output.name, primary.name, "left-of")
                },
                {
                    string.format("Above %s", primary.name),
                    xrandr_move(output.name, primary.name, "above")
                },
                {
                    string.format("Bellow %s", primary.name),
                    xrandr_move(output.name, primary.name, "below")
                },
                {
                    string.format("Mirror %s", primary.name),
                    xrandr_move(output.name, primary.name, "same-as")
                },
                {
                    "Make primray",
                    string.format("xrandr --output %s --primary", output.name)
                },
                {
                    "Switch Off",
                    string.format("xrandr --output %s --off", output.name)
                }
            }
        })
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
