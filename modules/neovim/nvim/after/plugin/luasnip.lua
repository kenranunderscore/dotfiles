require("luasnip.loaders.from_lua").load { paths = { "~/.config/nvim/lua/snippets/" } }

local ls = require "luasnip"
local map = vim.keymap.set
map({ "i", "s" }, "<tab>", function()
  ls.jump(1)
end)
map({ "i", "s" }, "<s-tab>", function()
  ls.jump(-1)
end)
