local function ensure_packer()
    local fn = vim.fn
    local install_path = fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"
    if fn.empty(fn.glob(install_path)) > 0 then
        fn.system { "git", "clone", "--depth", "1", "https://github.com/wbthomason/packer.nvim", install_path }
        vim.cmd("packadd packer.nvim")
        return true
    end
    return false
end

local packer_bootstrap = ensure_packer()

vim.cmd([[
  augroup packer_user_config
    autocmd!
    autocmd BufWritePost plugins.lua source <afile> | PackerInstall
  augroup end
]])

return require("packer").startup(function(use)
    use("wbthomason/packer.nvim")

    use("lewis6991/impatient.nvim")

    -- colorschemes
    use("bluz71/vim-moonfly-colors")
    use("rockerboo/boo-colorscheme-nvim")
    use("tiagovla/tokyodark.nvim")
    use("nvim-treesitter/nvim-treesitter")

    use("kyazdani42/nvim-web-devicons")

    use("ibhagwan/fzf-lua")

    use { "TimUntersberger/neogit", requires = "nvim-lua/plenary.nvim" }
    use("kylechui/nvim-surround")
    use("editorconfig/editorconfig-vim")

    use("neovim/nvim-lspconfig")
    use("hrsh7th/cmp-nvim-lsp")
    use("hrsh7th/cmp-buffer")
    use("hrsh7th/cmp-path")
    use("hrsh7th/nvim-cmp")
    use { "glepnir/lspsaga.nvim", branch = "main" }
    use("onsails/lspkind.nvim")

    use("l3mon4d3/luasnip")
    use("saadparwaiz1/cmp_luasnip")

    use("nvim-lualine/lualine.nvim")

    use("numtostr/comment.nvim")

    -- lispy languages
    use("Olical/conjure")
    use("Olical/aniseed")
    use("gpanders/nvim-parinfer")

    use("tpope/vim-repeat")
    use("ggandor/leap.nvim")

    if packer_bootstrap then
        require("packer").sync()
    end
end)