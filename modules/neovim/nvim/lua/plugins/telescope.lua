return {
  "nvim-telescope/telescope.nvim",
  tag = "0.1.8",
  cmd = "Telescope",
  dependencies = {
    "nvim-lua/plenary.nvim",
    { "nvim-telescope/telescope-fzf-native.nvim", build = "make" },
    { "nvim-telescope/telescope-ui-select.nvim" },
  },
  config = function()
    require("telescope").setup({
      defaults = {
        border = true,
        layout_strategy = "horizontal",
        file_ignore_patterns = { "^%.git/" },
      },
      pickers = {
        find_files = { previewer = false, layout_config = { height = 0.4 } },
        git_files = { previewer = false, layout_config = { height = 0.4 } },
        buffers = { previewer = false, layout_config = { height = 0.4 } },
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
    require("telescope").load_extension("ui-select")
  end,
  keys = {
    { "<leader>pf", "<cmd>Telescope find_files hidden=true<cr>", desc = "find file in project" },
    { "<leader>ps", "<cmd>Telescope live_grep<cr>", desc = "grep in project" },
    { "<leader>bb", "<cmd>Telescope buffers<cr>", desc = "switch buffer" },
  },
}
