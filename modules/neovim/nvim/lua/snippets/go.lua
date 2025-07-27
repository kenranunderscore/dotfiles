require("luasnip.session.snippet_collection").clear_snippets "go"

local luasnip = require "luasnip"
local s = luasnip.s
local t = luasnip.t
local i = luasnip.i

luasnip.add_snippets("go", {
  -- struct
  s("t", {
    t "type ",
    i(1, "name"),
    t " struct {",
    t { "", "\t" },
    i(0),
    t { "", "}" },
  }),

  -- return without error
  s("rr", {
    t "return ",
    i(0),
    t ", nil",
  }),

  -- for (index and item)
  s("ff", {
    t "for ",
    i(3, "i"),
    t ", ",
    i(2, "item"),
    t " := range ",
    i(1, "items"),
    t { " {", "\t" },
    i(0),
    t { "", "}" },
  }),

  -- for (index only)
  s("fi", {
    t "for i := range ",
    i(1, "until"),
    t { " {", "\t" },
    i(0),
    t { "", "}" },
  }),

  -- for each
  s("fe", {
    t "for _, ",
    i(2, "item"),
    t " := range ",
    i(1, "items"),
    t { " {", "\t" },
    i(0),
    t { "", "}" },
  }),

  -- if (err != nil)
  s("e", {
    t { "if err != nil {", "" },
    i(0),
    t { "", "}" },
  }),

  -- switch
  s("sw", {
    t "switch ",
    i(1, "thing"),
    t { " {", "case " },
    i(2, "foo"),
    t { ":", "\t" },
    i(0),
    t { "", "}" },
  }),
})
