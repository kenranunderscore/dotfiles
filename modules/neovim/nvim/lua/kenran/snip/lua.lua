local luasnip = require("luasnip")
local snippet = luasnip.snippet
local fmt = require("luasnip.extras.fmt").fmt
local insert_node = luasnip.insert_node

luasnip.add_snippets("lua", {
    snippet(
        {
            trig = "preq",
            dscr = {
                "Add Lua code to pcall(require, '<package>'), as well as",
                "print a message and return if the require fails.",
            },
        },
        fmt(
            [[
            local {package}_setup, {package} = pcall(require, "{package}")
            if not {package}_setup then
                print("Package '{package}' failed to load")
                return
            end
            ]],
            {
                package = insert_node(1, "package"),
            },
            {
                repeat_duplicates = true,
            }
        )
    ),
})
