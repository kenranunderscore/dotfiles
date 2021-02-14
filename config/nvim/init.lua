local execute = vim.api.nvim_command
local fn = vim.fn

local install_path = fn.stdpath('data')..'/site/pack/packer/opt/packer.nvim'
if fn.empty(fn.glob(install_path)) > 0 then
  execute('!git clone https://github.com/wbthomason/packer.nvim '..install_path)
end

vim.cmd [[packadd packer.nvim]]
vim.api.nvim_exec([[
  augroup Packer
    autocmd!
    autocmd BufWritePost plugins.lua PackerCompile
  augroup end
]], false)

local use = require('packer').use
require('packer').startup(function()
  use {'wbthomason/packer.nvim', opt = true}

  -- git/github
  use 'tpope/vim-fugitive'
  use 'tpope/vim-rhubarb'

  -- surround stuff with vim-style commands
  use 'tpope/vim-surround'

  -- fuzzy searching; still needs to be configured
  use 'nvim-telescope/telescope.nvim'
  use 'nvim-lua/plenary.nvim'
  use 'nvim-lua/popup.nvim'

  -- completion framework
  use 'hrsh7th/nvim-compe'

  -- nix expressions
  use 'LnL7/vim-nix'

  -- color theme
  use 'joshdick/onedark.vim'
  use 'morhetz/gruvbox'

  -- status bar
  use 'vim-airline/vim-airline'

  -- smooth scrolling for ^D, ^U, ...
  use 'psliwka/vim-smoothie'

  -- gl and gL to align text by characters
  use 'tommcdo/vim-lion'

  -- treesitter for better syntax highlighting
  use 'nvim-treesitter/nvim-treesitter'
end)

-- options
vim.o.autoindent = true
vim.o.ignorecase = true
vim.o.smartcase = true

-- space as leader key
vim.api.nvim_set_keymap('', '<space>', '<nop>', { noremap = true, silent = true })
vim.g.mapleader = ' '

-- colorscheme configuration
vim.o.termguicolors = true
-- onedark
-- vim.g.onedark_terminal_italics = 2
-- vim.cmd[[colorscheme onedark]]
--
-- gruvbox
vim.g.gruvbox_contrast_dark = 'hard'
vim.cmd[[colorscheme gruvbox]]

-- compe
vim.o.completeopt = "menu,menuone,noselect"
require('compe').setup {
  min_length = 3,
  source = {
    path = true,
    buffer = true,
    nvim_lsp = true,
    nvim_lua = true,
    treesitter = true
  }
}

-- telescope
vim.api.nvim_set_keymap('n', '<leader>ff', [[<cmd>lua require('telescope.builtin').find_files()<cr>]], { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>fb', [[<cmd>lua require('telescope.builtin').buffers()<cr>]], { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>fp', [[<cmd>lua require('telescope.builtin').git_files()<cr>]], { noremap = true, silent = true })

-- treesitter
require('nvim-treesitter.configs').setup {
  highlight = {
    enable = true
  },
}
