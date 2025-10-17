vim.g.mapleader = " "
vim.g.maplocalleader = ","

local opt = vim.opt

-- use relative line numbers
opt.nu = false
opt.relativenumber = false

-- indentation
opt.tabstop = 4
opt.softtabstop = 4
opt.shiftwidth = 4
opt.expandtab = true

-- always show the signcolumn to prevent horizontal buffer movement
opt.signcolumn = "yes"

opt.list = true
opt.listchars = "tab:» ,nbsp:+"

opt.smartcase = true
opt.ignorecase = true

opt.guicursor = "n-v-c:block-nCursor,i:block-Cursor"

opt.mousemodel = "extend"

vim.g["conjure#filetypes"] = { "clojure" }

-- have the statusline at the *very* bottom
opt.cmdheight = 0
opt.laststatus = 2

vim.opt.runtimepath:prepend "~/projects/naga.nvim"
