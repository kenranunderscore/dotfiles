return {
  "nvim-treesitter/nvim-treesitter",
  build = ":TSUpdate",
  config = function()
    require("nvim-treesitter.configs").setup {
      ensure_installed = {
        "clojure",
        "css",
        "elixir",
        "go",
        "haskell",
        "html",
        "javascript",
        "lua",
        "nix",
        "ocaml",
        "typescript",
        "vim",
        "vimdoc",
      },
      highlight = {
        enable = true,
        use_languagetree = true,
      },
      indent = { enable = true },
    }
  end,
}
