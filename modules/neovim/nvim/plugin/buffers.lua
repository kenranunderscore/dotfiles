require("mini.bufremove").setup()

vim.keymap.set({ "n", "v" }, "<leader>k", function()
  MiniBufremove.delete()
end)
