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
          fuzzy = false,
          override_generic_sorter = true,
          override_file_sorter = true,
          case_mode = "smart_case",
        },
      },
    })
    require("telescope").load_extension("fzf")
  end,
  keys = {
    { "<leader>pf", "<cmd>Telescope find_files<cr>", desc = "find file in project" },
    { "<leader>bb", "<cmd>Telescope buffers<cr>", desc = "switch buffer" },
  },
}
