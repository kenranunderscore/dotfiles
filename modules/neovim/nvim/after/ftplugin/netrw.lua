-- need to set this again here, as netrw replaces C-l with its own refresh.
-- not sure how to refresh netrw now though...

vim.keymap.set("n", "<C-l>", "<C-w><C-w>", { buffer = true })
