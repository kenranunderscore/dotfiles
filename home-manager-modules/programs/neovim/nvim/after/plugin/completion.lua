local cmp = require("cmp")
local luasnip = require("luasnip")

cmp.setup {
    completion = {
        keyword_length = 3,
    },
    snippet = {
        expand = function(args)
            luasnip.lsp_expand(args.body)
        end,
    },
    sources = cmp.config.sources({
        { name = "luasnip" },
        { name = "buffer" },
        { name = "path" },
    }),
    mapping = cmp.mapping.preset.insert({
        ["<down>"] = cmp.mapping.select_next_item(),
        ["<tab>"] = cmp.mapping.select_next_item(),
        ["<up>"] = cmp.mapping.select_prev_item(),
        ["<cr>"] = cmp.mapping.confirm({ select = false }),
        ["<C-b>"] = cmp.mapping.scroll_docs(-4),
        ["<C-f>"] = cmp.mapping.scroll_docs(4),
        ["<C-space>"] = cmp.mapping.complete(),
    }),
}
