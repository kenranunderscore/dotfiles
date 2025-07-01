return {
  "neovim/nvim-lspconfig",
  config = function()
    vim.lsp.enable("lua_ls")

    vim.lsp.enable("elixirls")
    vim.lsp.config("elixirls", { cmd = { "elixir-ls" } })
  end,
}
