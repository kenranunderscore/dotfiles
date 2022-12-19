local impatient_setup, _ = pcall(require, "impatient")
if not impatient_setup then
    print("Package 'impatient' failed to load")
end

-- These settings should be loaded before everything else.  Hopefully that's
-- the case.

-- SPC as leader key
vim.g.mapleader = " "
vim.g.maplocalleader = ","

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

opt.signcolumn = "yes"

opt.completeopt = "menu,menuone,noselect"

vim.g.autoformat = 1

opt.exrc = true

-- Show some special characters, like carriage returns, tabs etc.
opt.list = true
opt.listchars = "tab:» ,nbsp:+,eol:⏎"

-- Load the plugins
-- NOTE: can I solve the bootstrapping problem with aniseed?  Maybe shipping
-- aniseed with neovim via nix is an option.
require("kenran.plugins")
