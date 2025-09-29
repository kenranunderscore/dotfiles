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
local capabilities = require("mini.completion").get_lsp_capabilities {}
local default_opts = { capabilities = capabilities, autostart = false }

-- Lua
lsp.config("lua_ls", default_opts)
lsp.enable "lua_ls"

-- Nix
lsp.config("nil_ls", default_opts)
lsp.enable "nil_ls"

-- Rust
lsp.config("rust_analyzer", {
  capabilities = capabilities,
  autostart = false,
  settings = { ["rust-analyzer"] = {
    cargo = { allFeatures = true },
  } },
})
lsp.enable "rust_analyzer"

-- HTML/Emmet
lsp.config("emmet-language-server", default_opts)
lsp.enable "emmet-language-server"

-- Python
lsp.config("pylsp", default_opts)
lsp.enable "pylsp"

-- Clojure
lsp.config("clojure_lsp", default_opts)
lsp.enable "clojure_lsp"
