return {
  { "miikanissi/modus-themes.nvim", lazy = false, event = "VeryLazy" },
  { "navarasu/onedark.nvim", lazy = false, event = "VeryLazy", opts = { style = "deep" } },
  { "shaunsingh/solarized.nvim", lazy = false, event = "VeryLazy" },
  { "eldritch-theme/eldritch.nvim", lazy = false, event = "VeryLazy" },
  {
    "bluz71/vim-moonfly-colors",
    lazy = false,
    event = "VeryLazy",
    config = function()
      vim.cmd "colorscheme moonfly"
    end,
  },
}
