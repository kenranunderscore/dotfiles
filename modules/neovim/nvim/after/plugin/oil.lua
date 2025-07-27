require("oil").setup {
  keymaps = {
    ["<C-s>"] = false,
  },
}

vim.keymap.set({ "n", "v" }, "<leader>dd", "<cmd>Oil<cr>")
