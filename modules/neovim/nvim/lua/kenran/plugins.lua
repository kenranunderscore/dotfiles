-- Plugin management
vim.api.nvim_create_augroup("packer_user_config", {})
vim.api.nvim_create_autocmd("BufWritePost", {
    pattern = "plugins.lua",
    group = "packer_user_config",
    callback = function(args)
        vim.cmd("source " .. args.file)
        vim.cmd("PackerInstall")
    end,
})

local packer = require("packer")
local use = packer.use

packer.reset()
packer.init {
    -- FIXME: loading of snapshots doesn't really work yet; also don't know the
    -- workflow yet, as the interactive commands still need an argument, which
    -- I'd like to be taken from these configured value automatically. maybe
    -- have to write my own Lua function
    snapshot_path = vim.fn.stdpath("config"),
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
use{ "catppuccin/nvim", as = "catppuccin" }
use("sainnhe/everforest")

-- General utils/quality of life
use("tpope/vim-repeat")
use("tpope/vim-unimpaired")
use("tpope/vim-vinegar")
use("tpope/vim-eunuch")
use("kylechui/nvim-surround")
use("numtostr/comment.nvim")
use("lukas-reineke/indent-blankline.nvim")

-- Fuzzy search interface
use("nvim-telescope/telescope.nvim")
use { "nvim-telescope/telescope-fzf-native.nvim", run = "make" }
use("nvim-telescope/telescope-ui-select.nvim")

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
