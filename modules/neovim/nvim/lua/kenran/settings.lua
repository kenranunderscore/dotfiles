-- SPC as leader key
vim.g.mapleader = " "
vim.g.maplocalleader = ","

local opt = vim.opt

-- Use relative line numbers
opt.nu = true
opt.relativenumber = true

-- 4 spaces as default; use exrc to diverge locally
opt.tabstop = 4
opt.softtabstop = 4
opt.shiftwidth = 4
opt.expandtab = true

opt.hlsearch = true
opt.incsearch = true
opt.ignorecase = true
opt.smartcase = true

opt.wrap = false

opt.guicursor = ""
opt.termguicolors = true

opt.signcolumn = "yes"

opt.completeopt = "menu,menuone,noselect"

-- Skip auto-formatting for now, as formatprg doesn't always behave as expected
vim.g.autoformat = 0

-- .dir-locals.el-ish functionality
opt.exrc = true

-- Show some special characters, like carriage returns, tabs etc.
opt.list = true
opt.listchars = "tab:» ,nbsp:+,eol:⏎"

-- Don't continue comments when inserting new lines with `o` (hacky, but seems
-- like the only way, from a discussion in Matrix)
vim.api.nvim_create_autocmd("FileType", {
    callback = function()
        vim.opt.formatoptions:remove("o")
    end,
})
