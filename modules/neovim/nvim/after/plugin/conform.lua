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
  format_on_save = {
    timeout_ms = 500,
    lsp_format = "fallback",
  },
}
