return {
  "tpope/vim-fugitive",
  cmd = { "G" },
  keys = {
    { "<leader>gs", "<cmd>G<cr>", desc = "git status" },
    { "<leader>gp", "<cmd>G push<cr>", desc = "git push" },
    { "<leader>gl", "<cmd>G pull<cr>", desc = "git pull" },
  },
}
