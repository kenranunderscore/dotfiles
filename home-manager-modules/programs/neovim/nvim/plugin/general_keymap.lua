local nnoremap = require("kenran.remap").nnoremap

nnoremap("<leader>pd", "<cmd>Ex<cr>")
nnoremap("<C-u>", "<C-u>zz")
nnoremap("<C-d>", "<C-d>zz")
nnoremap("n", "nzz")
nnoremap("N", "Nzz")
nnoremap("*", "*zz")
nnoremap("#", "#zz")

-- quickly delete the current buffer
nnoremap("<leader>bq", "<cmd>bd<cr>")

-- remove search highlighting on ESC
nnoremap("<esc>", "<cmd>noh<cr>")

-- windows
nnoremap("<C-l>", "<C-w><C-w>")
nnoremap("<C-w><C-d>", "<C-w>q")
