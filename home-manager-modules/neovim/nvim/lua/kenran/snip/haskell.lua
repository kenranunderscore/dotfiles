local luasnip = require("luasnip")
local snippet = luasnip.snippet
local fmt = require("luasnip.extras.fmt").fmt
local text_node = luasnip.text_node
local insert_node = luasnip.insert_node

local haskell_impl_with_error = 'error "TODO: implement"'
luasnip.add_snippets("haskell", {
    snippet("ds", {
        text_node("deriving stock (Eq, Show)"),
    }),
    snippet("dsg", {
        text_node("deriving stock (Eq, Show, Generic)"),
    }),
    snippet("impl", {
        text_node(haskell_impl_with_error),
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
            {name} {arguments} =
                {impl}
            ]],
            {
                name = insert_node(1, "functionName"),
                types = insert_node(2, "type"),
                arguments = insert_node(3, "args"),
                impl = insert_node(0, haskell_impl_with_error),
            },
            { repeat_duplicates = true }
        )
    ),
    snippet(
        "inst",
        fmt(
            [[
            instance {typeclass} {type} where
                {method} = {body}
            ]],
            {
                typeclass = insert_node(1, "typeclass"),
                type = insert_node(2, "type"),
                method = insert_node(3, "method"),
                body = insert_node(0, haskell_impl_with_error),
            },
            { repeat_duplicates = true }
        )
    ),
})
