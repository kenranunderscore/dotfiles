require("telescope").setup {
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
}

-- FIXME: run "cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release" via vim.pick somehow
require("telescope").load_extension "fzf"
require("telescope").load_extension "ui-select"

vim.keymap.set({ "n", "v" }, "<leader>pf", "<cmd>Telescope find_files hidden=true<cr>")
vim.keymap.set({ "n", "v" }, "<leader>ps", "<cmd>Telescope live_grep<cr>")
vim.keymap.set({ "n", "v" }, "<leader>bb", "<cmd>buffers<cr>")
