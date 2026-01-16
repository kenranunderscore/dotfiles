require("flash").setup {
  jump = { autojump = false },
  modes = { search = { enabled = false } },
}

vim.keymap.set({ "n", "x", "o" }, "s", function()
  require("flash").jump()
end, { desc = "Flash" })
