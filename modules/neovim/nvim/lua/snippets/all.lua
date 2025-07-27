require("luasnip.session.snippet_collection").clear_snippets "all"

local luasnip = require "luasnip"
local s = luasnip.s
local t = luasnip.t

luasnip.add_snippets("all", {
  s("td", { t "TODO: " }),
  s("tdj", { t "TODO(Johannes): " }),
  s("fm", { t "FIXME: " }),
  s("fmj", { t "FIXME(Johannes): " }),
})
