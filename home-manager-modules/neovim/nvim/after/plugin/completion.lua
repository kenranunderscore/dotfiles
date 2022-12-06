local setup, cmp = pcall(require, "cmp")
if not setup then
    print("Loading nvim-cmp failed")
    return
end

local luasnip = require("luasnip")
local lspkind = require("lspkind")

local border = "single"

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
        { name = "nvim_lsp" },
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
    formatting = {
        format = lspkind.cmp_format({
            maxwidth = 50,
            ellipsis_char = "...",
        })
    },
    window = {
        completion = {
            border = border,
        },
        documentation = {
            border = border,
        },
    },
}
