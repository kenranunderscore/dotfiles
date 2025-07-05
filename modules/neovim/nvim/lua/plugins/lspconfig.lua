return {
  "neovim/nvim-lspconfig",
  lazy = false,
  dependencies = {
    { "ms-jpq/coq_nvim", branch = "coq" },
    { "ms-jpq/coq.artifacts", branch = "artifacts" },
    { "maan2003/lsp_lines.nvim" },
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
    require("lsp_lines").setup()
    vim.diagnostic.config({
      virtual_text = true,
      virtual_lines = false,
    })
    vim.keymap.set("n", "<leader>ll", function()
      local cfg = vim.diagnostic.config() or {}
      if cfg.virtual_text then
        vim.diagnostic.config({ virtual_text = false, virtual_lines = true })
      else
        vim.diagnostic.config({ virtual_text = true, virtual_lines = false })
      end
    end, { desc = "toggle lsp_lines" })

    vim.lsp.inlay_hint.enable(true)
    vim.keymap.set("n", "<leader>li", function()
      vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled({ bufnr = 0 }), { bufnr = 0 })
    end)

    local coq = require("coq")
    local lsp = vim.lsp

    -- Lua
    lsp.config("lua_ls", coq.lsp_ensure_capabilities({}))
    lsp.enable("lua_ls")

    -- Nix
    lsp.config(
      "nil_ls",
      coq.lsp_ensure_capabilities({
        autostart = true,
        cmd = { "nil" },
      })
    )
    lsp.enable("nil_ls")

    -- Rust
    lsp.config("rust_analyzer", coq.lsp_ensure_capabilities({}))
    lsp.enable("rust_analyzer")

    -- HTML/Emmet
    lsp.config(
      "emmet-language-server",
      coq.lsp_ensure_capabilities({
        cmd = { "emmet-language-server", "--stdio" },
      })
    )
    lsp.enable("emmet-language-server")
  end,
}
