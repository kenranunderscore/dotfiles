-- thanks to TJ's series I finally managed to get started porting my Emacs
-- yasnippet snippets over to LuaSnip

local result, luasnip = pcall(require, "luasnip")
if not result then
    return
end

local snippet = luasnip.snippet
local text_node = luasnip.text_node
-- local dynamic_node = luasnip.dynamic_node
local insert_node = luasnip.insert_node
local fmt = require("luasnip.extras.fmt").fmt

-- try C-k for snippet expansion
vim.keymap.set({ "i", "s" }, "<C-k>", function()
    if luasnip.expand_or_jumpable() then
        luasnip.expand_or_jump()
    end
end, { silent = true })

-- use C-j for snippet expansion
vim.keymap.set({ "i", "s" }, "<C-j>", function()
    luasnip.jump(-1)
end, { silent = true })

luasnip.config.set_config {
    history = false,
    updateevents = "TextChanged,TextChangedI",
    -- FIXME(Johannes): see if I really need this
    enable_autosnippets = true,
}

luasnip.add_snippets("all", {
    snippet("fm", {
        text_node("FIXME(Johannes): "),
    }),
    snippet("td", {
        text_node("TODO(Johannes): "),
    }),
    snippet("note", {
        text_node("NOTE(Johannes): "),
    }),
})

luasnip.add_snippets("haskell", {
    snippet("ds", {
        text_node("deriving stock (Eq, Show)"),
    }),
    snippet("dsg", {
        text_node("deriving stock (Eq, Show, Generic)"),
    }),
    snippet("impl", {
        text_node('error "TODO: implement this"'),
    }),
    snippet(
        "nt",
        fmt("newtype {name} = {name} {{ un{name} :: {type} }}", {
            name = insert_node(1, "Name"),
            type = insert_node(2, "Type"),
        }, {
            repeat_duplicates = true,
        })
    ),
    snippet(
        "fn",
        fmt(
            [[
            {name} :: {types}
            {name} =
                {impl}
            ]],
            {
                name = insert_node(1, "functionName"),
                types = insert_node(2, "a"),
                impl = insert_node(0, 'error "TODO: implement this"'),
            },
            { repeat_duplicates = true }
        )
    ),
})

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
