local map = vim.keymap.set

map("n", "<esc>", "<cmd>noh<cr>")
map({ "n", "i" }, "<C-s>", "<esc><cmd>w<cr>")
map({ "n", "i" }, "<C-l>", "<C-w><C-w>")
map("n", "<C-w><C-d>", "<C-w>q")
map("n", "gd", vim.lsp.buf.definition)
