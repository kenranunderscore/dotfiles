local cmp = require("cmp")

cmp.setup {
    sources = cmp.config.sources(
    { { name = "nvim_lsp" }, { name = "vsnip" } },
    { name = "buffer" })
}
