return {
  "nvim-telescope/telescope.nvim",
  tag = "0.1.8",
  cmd = "Telescope",
  dependencies = {
    "nvim-lua/plenary.nvim",
    { "nvim-telescope/telescope-fzf-native.nvim", build = "make" },
  },
  config = function()
    local borderchars = { "─", "│", "─", "│", "┌", "┐", "┘", "└" }
    require("telescope").setup({
      defaults = {
        border = true,
        borderchars = borderchars,
        layout_strategy = "horizontal",
      },
      extensions = {
        fzf = {
          override_generic_sorter = true,
          override_file_sorter = true,
          case_mode = "smart_case",
        },
      },
    })
    require("telescope").load_extension("fzf")
    require("plugins.config.telescope")
  end,
}
