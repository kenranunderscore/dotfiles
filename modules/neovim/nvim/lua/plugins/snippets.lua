return {
  "l3mon4d3/luasnip",
  build = "make install_jsregexp",
  config = function()
    require("luasnip.loaders.from_lua").load { paths = { "~/.config/nvim/lua/snippets/" } }

    local ls = require "luasnip"
    local map = vim.keymap.set
    map({ "i", "s" }, "<tab>", function()
      ls.jump(1)
    end, { silent = true })
    map({ "s", "i" }, "<s-tab>", function()
      ls.jump(-1)
    end, { silent = true })
  end,
}
