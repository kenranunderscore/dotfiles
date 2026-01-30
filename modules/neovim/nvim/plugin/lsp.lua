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

local lsp = vim.lsp
lsp.inlay_hint.enable(false)
vim.keymap.set("n", "<leader>li", function()
  lsp.inlay_hint.enable(not lsp.inlay_hint.is_enabled { bufnr = 0 }, { bufnr = 0 })
end)

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

-- Nickel
lsp.enable "nickel_ls"

-- Zig
lsp.enable "zls"

-- Set up document highlights
vim.opt.updatetime = 500
vim.api.nvim_create_autocmd("LspAttach", {
  callback = function(args)
    local client = vim.lsp.get_client_by_id(args.data.client_id)
    if client and client.server_capabilities.documentHighlightProvider then
      vim.api.nvim_create_autocmd(
        { "CursorHold", "CursorHoldI" },
        { buffer = args.buf, callback = vim.lsp.buf.document_highlight }
      )
      vim.api.nvim_create_autocmd(
        { "CursorMoved", "CursorMovedI" },
        { buffer = args.buf, callback = vim.lsp.buf.clear_references }
      )
    end
  end,
})
