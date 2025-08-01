require("oil").setup {
  view_options = {
    show_hidden = true,
    is_always_hidden = function(name, _)
      return name == ".."
    end,
  },
  float = {
    padding = 10,
    max_width = 0.5,
    max_height = 0.5,
    border = "rounded",
  },
  keymaps = {
    ["<C-s>"] = false,
    ["q"] = function()
      require("oil").close()
    end,
  },
}

vim.keymap.set({ "n", "v" }, "-", function()
  require("oil").open_float()
end)
