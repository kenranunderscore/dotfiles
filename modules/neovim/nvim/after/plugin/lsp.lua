require("lazydev").setup {
  library = {
    { path = "${3rd}/luv/library", words = { "vim%.uv" } },
  },
}

vim.diagnostic.config {
  virtual_text = true,
  virtual_lines = false,
}
vim.keymap.set("n", "<leader>ll", function()
  local cfg = vim.diagnostic.config() or {}
  if cfg.virtual_text then
    vim.diagnostic.config { virtual_text = false, virtual_lines = true }
  else
    vim.diagnostic.config { virtual_text = true, virtual_lines = false }
  end
end, { desc = "toggle lsp_lines" })

vim.lsp.inlay_hint.enable(true)
vim.keymap.set("n", "<leader>li", function()
  vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled { bufnr = 0 }, { bufnr = 0 })
end)

local lsp = vim.lsp
local capabilities = require("cmp_nvim_lsp").default_capabilities()

-- Lua
lsp.config("lua_ls", { capabilities = capabilities })
lsp.enable "lua_ls"

-- Nix
lsp.config("nil_ls", { capabilities = capabilities })
lsp.enable "nil_ls"

-- Rust
lsp.config("rust_analyzer", { capabilities = capabilities })
lsp.enable "rust_analyzer"

-- HTML/Emmet
lsp.config("emmet-language-server", { capabilities = capabilities })
lsp.enable "emmet-language-server"

-- Python
lsp.config("pylsp", { capabilities = capabilities })
lsp.enable "pylsp"

-- Clojure
lsp.config("clojure_lsp", { capabilities = capabilities })
lsp.enable "clojure_lsp"
