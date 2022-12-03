-- vertically center the current line after certain jumps (thanks prime)
vim.keymap.set("n", "<C-u>", "<C-u>zz")
vim.keymap.set("n", "<C-d>", "<C-d>zz")
vim.keymap.set("n", "n", "nzz")
vim.keymap.set("n", "N", "Nzz")
vim.keymap.set("n", "*", "*zz")
vim.keymap.set("n", "#", "#zz")
vim.keymap.set("n", "G", "Gzz")

-- quickly delete the current buffer
vim.keymap.set("n", "<leader>bq", "<cmd>bd<cr>")

-- open netrw in the current pwd
vim.keymap.set("n", "<leader>pd", "<cmd>Ex<cr>")

-- remove search highlighting on ESC
vim.keymap.set("n", "<esc>", "<cmd>noh<cr>")

-- windows
vim.keymap.set("n", "<C-l>", "<C-w><C-w>")
vim.keymap.set("n", "<C-w><C-d>", "<C-w>q")
