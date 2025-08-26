return {
  "tpope/vim-fugitive",
  cmd = { "G" },
  keys = {
    { "<leader>gs", "<cmd>G<cr>", desc = "Git status" },
    { "<leader>gp", "<cmd>G push<cr>", desc = "Git push" },
    { "<leader>gl", "<cmd>G pull<cr>", desc = "Git pull" },
  },
}
