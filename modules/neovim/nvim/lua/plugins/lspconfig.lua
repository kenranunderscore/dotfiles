return {
  "neovim/nvim-lspconfig",
  lazy = false,
  dependencies = {
    { "ms-jpq/coq_nvim", branch = "coq" },
    { "ms-jpq/coq.artifacts", branch = "artifacts" },
    {
      "folke/lazydev.nvim",
      ft = "lua", -- only load on lua files
      opts = {
        library = {
          -- See the configuration section for more details
          -- Load luvit types when the `vim.uv` word is found
          { path = "${3rd}/luv/library", words = { "vim%.uv" } },
        },
      },
    },
  },
  init = function()
    vim.g.coq_settings = {
      auto_start = false,
    }
  end,
  config = function()
    vim.diagnostic.config({
      virtual_text = true,
      virtual_lines = false,
    })

    local coq = require("coq")
    local lsp = vim.lsp

    -- Lua
    lsp.config("lua_ls", coq.lsp_ensure_capabilities({}))
    lsp.enable("lua_ls")

    -- Elixir
    lsp.config(
      "elixirls",
      coq.lsp_ensure_capabilities({
        autostart = true,
        cmd = { "elixir-ls" },
      })
    )
    lsp.enable("elixirls")

    -- Nix
    lsp.config(
      "nil_ls",
      coq.lsp_ensure_capabilities({
        autostart = true,
        cmd = { "nil" },
      })
    )
    lsp.enable("nil_ls")
  end,
}
