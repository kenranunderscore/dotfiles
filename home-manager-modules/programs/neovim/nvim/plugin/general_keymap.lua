local nnoremap = require("kenran.remap").nnoremap

-- vertically center the current line after certain jumps (thanks prime)
nnoremap("<C-u>", "<C-u>zz")
nnoremap("<C-d>", "<C-d>zz")
nnoremap("n", "nzz")
nnoremap("N", "Nzz")
nnoremap("*", "*zz")
nnoremap("#", "#zz")
nnoremap("G", "Gzz")

-- quickly delete the current buffer
nnoremap("<leader>bq", "<cmd>bd<cr>")

-- open netrw in the current pwd
nnoremap("<leader>pd", "<cmd>Ex<cr>")

-- remove search highlighting on ESC
nnoremap("<esc>", "<cmd>noh<cr>")

-- windows
nnoremap("<C-l>", "<C-w><C-w>")
nnoremap("<C-w><C-d>", "<C-w>q")
