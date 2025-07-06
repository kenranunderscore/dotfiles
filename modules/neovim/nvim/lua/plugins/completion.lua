return {
  "hrsh7th/nvim-cmp",
  event = "VeryLazy",
  dependencies = {
    { "hrsh7th/cmp-nvim-lsp" },
    { "l3mOn4d3/luasnip" },
    { "saadparwaiz1/cmp_luasnip" },
    { "hrsh7th/cmp-path" },
    { "hrsh7th/cmp-buffer" },
  },
  config = function()
    local cmp = require("cmp")
    cmp.setup({
      snippet = {
        expand = function(args)
          require("luasnip").lsp_expand(args.body)
        end,
      },
      mapping = cmp.mapping.preset.insert({
        ["<c-f>"] = cmp.mapping.scroll_docs(4),
        ["<c-b>"] = cmp.mapping.scroll_docs(-4),
        ["<c-space>"] = cmp.mapping.complete(),
        ["<tab>"] = cmp.mapping.select_next_item(),
        ["<s-tab>"] = cmp.mapping.select_prev_item(),
        ["<cr>"] = cmp.mapping.confirm({ select = true }),
      }),
      window = {
        completion = cmp.config.window.bordered(),
        documentation = cmp.config.window.bordered(),
      },
      sources = cmp.config.sources({
        { name = "nvim_lsp" },
        { name = "luasnip" },
        { name = "buffer" },
      }),
    })
  end,
}
