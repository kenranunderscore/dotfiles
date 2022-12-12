-- thanks to TJ's series I finally managed to get started porting my Emacs
-- yasnippet snippets over to LuaSnip

local result, luasnip = pcall(require, "luasnip")
if not result then
    return
end

local snippet = luasnip.snippet
local text_node = luasnip.text_node

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

-- language-specific snippets are defined separately
require("kenran.snip.haskell")
require("kenran.snip.lua")
