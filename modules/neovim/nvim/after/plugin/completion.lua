local cmp = require "cmp"
cmp.setup {
  snippet = {
    expand = function(args)
      require("luasnip").lsp_expand(args.body)
    end,
  },
  mapping = cmp.mapping.preset.insert {
    ["<c-f>"] = cmp.mapping.scroll_docs(4),
    ["<c-b>"] = cmp.mapping.scroll_docs(-4),
    ["<c-space>"] = cmp.mapping.complete(),
    ["<tab>"] = cmp.mapping.select_next_item(),
    ["<s-tab>"] = cmp.mapping.select_prev_item(),
    ["<cr>"] = cmp.mapping.confirm { select = true },
  },
  window = {
    completion = cmp.config.window.bordered(),
    documentation = cmp.config.window.bordered(),
  },
  sources = cmp.config.sources {
    { name = "nvim_lsp" },
    { name = "luasnip" },
    { name = "buffer" },
  },
}
