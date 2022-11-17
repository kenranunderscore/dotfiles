-- SPC as leader key
vim.g.mapleader = " "

local opt = vim.opt

opt.nu = true
opt.relativenumber = true

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

opt.completeopt = "menu,menuone,noselect"
