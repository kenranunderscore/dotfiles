require("luasnip.session.snippet_collection").clear_snippets "go"

local luasnip = require "luasnip"
local s = luasnip.s
local t = luasnip.t
local i = luasnip.i

luasnip.add_snippets("elixir", {
  s("m", {
    t "defmodule ",
    i(1, "TheModule"),
    t { " do", "\t" },
    i(0),
    t { "", "end" },
  }),
})
