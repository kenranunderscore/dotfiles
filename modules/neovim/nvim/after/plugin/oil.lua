require("oil").setup {
  view_options = {
    show_hidden = true,
    is_always_hidden = function(name, _)
      return name == ".."
    end,
  },
  keymaps = {
    ["<C-s>"] = false,
  },
}

vim.keymap.set({ "n", "v" }, "<leader>dd", "<cmd>Oil<cr>")
