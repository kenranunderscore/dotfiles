return {
  "neovim/nvim-lspconfig",
  config = function()
    local lsp = vim.lsp
    lsp.enable("lua_ls")

    lsp.enable("elixirls")
    lsp.config("elixirls", { cmd = { "elixir-ls" } })

    lsp.enable("nil_ls")
    lsp.config("nil_ls", {
        autostart = true,
        cmd = { "nil" },
    })
  end,
}
