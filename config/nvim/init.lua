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

local use = require'packer'.use
require'packer'.startup(function()
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

  -- colorschemes
  use 'joshdick/onedark.vim'
  use 'srcery-colors/srcery-vim'
  use 'sainnhe/sonokai'

  -- status bar
  use 'itchyny/lightline.vim'

  -- smooth scrolling for ^D, ^U, ...
  use 'psliwka/vim-smoothie'

  -- gl and gL to align text by characters
  use 'tommcdo/vim-lion'

  -- treesitter for better syntax highlighting
  use 'nvim-treesitter/nvim-treesitter'
  use 'nvim-treesitter/playground'

  -- beautiful icons
  use 'kyazdani42/nvim-web-devicons'

  -- better shell commands
  use 'tpope/vim-eunuch'

  -- less hassle with tab/indentation size
  use 'tpope/vim-sleuth'

  -- code formatter
  use 'sbdchd/neoformat'

  -- language-specific highlighting
  -- (Haskell does not work well with treesitter yet, for instance)
  use 'sheerun/vim-polyglot'

  -- automatically close parens
  use 'jiangmiao/auto-pairs'

  -- LSP
  use 'neovim/nvim-lspconfig'
  use 'nvim-lua/lsp_extensions.nvim'
  use 'kosayoda/nvim-lightbulb'
end)

-- options
vim.o.autoindent = true
vim.o.ignorecase = true
vim.o.smartcase = true
vim.o.expandtab = true
vim.o.autoindent = true
vim.wo.number = true

-- space as leader key
vim.api.nvim_set_keymap('', '<space>', '<nop>', { noremap = true, silent = true })
vim.g.mapleader = ' '

-- colorscheme configuration
vim.o.termguicolors = true

-- onedark
vim.g.onedark_terminal_italics = 2
-- vim.cmd [[colorscheme onedark]]

-- sonokai
vim.g.sonokai_style = 'maia'
-- vim.cmd [[colorscheme sonokai]]

-- srcery-dark
vim.cmd[[colorscheme srcery]]

-- lightline
vim.g.lightline = {
  colorscheme = 'srcery_drk';
}

-- compe
vim.o.completeopt = "menu,menuone,noselect"
require'compe'.setup {
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
vim.api.nvim_set_keymap('n', '<leader>ff', [[<cmd>lua require'telescope.builtin'.find_files()<cr>]], { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>fb', [[<cmd>lua require'telescope.builtin'.buffers()<cr>]], { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>fp', [[<cmd>lua require'telescope.builtin'.git_files()<cr>]], { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>fc', [[<cmd>lua require'telescope.builtin'.lsp_code_actions()<cr>]], { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>fo', [[<cmd>lua require'telescope.builtin'.oldfiles()<cr>]], { noremap = true, silent = true })
-- exit telescope via <esc>
require'telescope'.setup {
  defaults = {
    mappings = {
      i = {
        ["<esc>"] = require'telescope.actions'.close
      }
    }
  }
}

-- treesitter
require'nvim-treesitter.configs'.setup {
  ensure_installed = {
    bash,
    html,
    json,
    lua
  },
  highlight = {
    enable = true
  },
}

-- code formatting
vim.api.nvim_exec([[
  augroup fmt
    autocmd!
    autocmd BufWritePre * undojoin | Neoformat
  augroup end
]], false)

-- LSP
vim.cmd [[autocmd CursorHold,CursorHoldI * lua require'nvim-lightbulb'.update_lightbulb()]]
local lspconfig = require'lspconfig'
lspconfig.hls.setup {
  cmd = {"haskell-language-server", "--lsp"};
  root_dir = lspconfig.util.root_pattern("cabal.project");
}
