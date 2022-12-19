-- vertically center the current line after certain jumps (thanks prime)
vim.keymap.set("n", "<C-u>", "<C-u>zz")
vim.keymap.set("n", "<C-d>", "<C-d>zz")
vim.keymap.set("n", "n", "nzz")
vim.keymap.set("n", "N", "Nzz")
vim.keymap.set("n", "*", "*zz")
vim.keymap.set("n", "#", "#zz")
vim.keymap.set("n", "G", "Gzz")
vim.keymap.set("n", "<C-i>", "<C-i>zz")
vim.keymap.set("n", "<C-o>", "<C-o>zz")

-- quickly delete the current buffer
vim.keymap.set("n", "<leader>bq", "<cmd>b#|bd#<cr>")

-- open netrw in the current pwd
vim.keymap.set("n", "<leader>pd", "<cmd>Ex<cr>")

-- remove search highlighting on ESC
vim.keymap.set("n", "<esc>", "<cmd>noh<cr>")

-- windows
vim.keymap.set("n", "<C-l>", "<C-w><C-w>")
vim.keymap.set("n", "<C-w><C-d>", "<C-w>q")

-- format the whole file
vim.keymap.set({ "n", "v" }, "<leader>bf", "gggqG")

-- heresy: alternatively save with C-x C-s
vim.keymap.set({ "n", "v" }, "<C-x><C-s>", "<cmd>w<cr>")
