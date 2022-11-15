require("nvim-treesitter.configs").setup {
    ensure_installed = "all",
    sync_install = false,
    auto_install = false,
    ignore_install = {},
    highlight = {
        enable = true,
    },
}
