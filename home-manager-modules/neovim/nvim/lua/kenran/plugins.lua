-- Plugin management
vim.cmd([[
  augroup packer_user_config
    autocmd!
    autocmd BufWritePost plugins.lua source <afile> | PackerInstall
  augroup end
]])

local packer = require("packer")
local use = packer.use

packer.reset()
packer.init {
    -- FIXME: loading of snapshots doesn't really work yet; also don't know the
    -- workflow yet, as the interactive commands still need an argument, which
    -- I'd like to be taken from these configured value automatically. maybe
    -- have to write my own Lua function
    snapshot_path = "$HOME/.config/nvim",
    snapshot = "packer.lock",
    autoremove = true,
}

use("wbthomason/packer.nvim")

-- Speed up the startup of neovim
use("lewis6991/impatient.nvim")

-- Used in almost all Lua plugins
use("nvim-lua/plenary.nvim")

-- Colorschemes I like to use (TODO(Johannes): add modus maybe?)
use("tiagovla/tokyodark.nvim")

-- General utils/quality of life
use("tpope/vim-repeat")
use("tpope/vim-unimpaired")
use("tpope/vim-vinegar")
use("tpope/vim-eunuch")
use("kylechui/nvim-surround")
use("numtostr/comment.nvim")
use("lukas-reineke/indent-blankline.nvim")

-- Fuzzy search interface via fzf, like in the shell
use("ibhagwan/fzf-lua")

-- Git integration
use("tpope/vim-fugitive")
use("lewis6991/gitsigns.nvim")

-- Cool snippets, with completion
use("l3mon4d3/luasnip")
use("saadparwaiz1/cmp_luasnip")

-- Working in projects that use EditorConfig
use("editorconfig/editorconfig-vim")

-- LSP stuff, autocompletion, treesitter
use("neovim/nvim-lspconfig")
use("hrsh7th/cmp-nvim-lsp")
use("hrsh7th/cmp-buffer")
use("hrsh7th/cmp-path")
use("hrsh7th/nvim-cmp")
use("onsails/lspkind.nvim")
use("nvim-treesitter/nvim-treesitter")
use("kyazdani42/nvim-web-devicons")

-- Status line
use("nvim-lualine/lualine.nvim")

-- Clever jumping/sniping with s/S
use("ggandor/leap.nvim")

-- Dhall
use("vmchale/dhall-vim")

-- Lisps
use("Olical/conjure")
use("gpanders/nvim-parinfer")

use("gpanders/nvim-moonwalk")
