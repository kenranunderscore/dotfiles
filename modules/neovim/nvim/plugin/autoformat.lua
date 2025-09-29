require("conform").setup {
  formatters_by_ft = {
    elixir = { "mix" },
    go = { "gofmt" },
    haskell = { "fourmolu" },
    lua = { "stylua" },
    nix = { "nixfmt" },
    ocaml = { "ocamlformat" },
    python = { "isort", "black" },
    rust = { "rustfmt", lsp_format = "fallback" },
    zig = { "zigfmt" },
    clojure = { "cljfmt" },
  },
  format_on_save = function(bufnr)
    if vim.b[bufnr].disable_autoformat then
      return
    end
    return {
      timeout_ms = 500,
      lsp_format = "fallback",
    }
  end,
}
