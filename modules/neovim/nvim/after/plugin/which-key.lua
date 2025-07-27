vim.keymap.set({ "n", "v" }, "<leader>?", function()
  require("which-key").show { global = false }
end)
