-- Clone 'mini.nvim' manually in a way that it gets managed by 'mini.deps'
local path_package = vim.fn.stdpath "data" .. "/site/"
local mini_path = path_package .. "pack/deps/start/mini.nvim"
if not vim.uv.fs_stat(mini_path) then
  vim.cmd 'echo "Installing `mini.nvim`" | redraw'
  local clone_cmd = {
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/nvim-mini/mini.nvim",
    mini_path,
  }
  vim.fn.system(clone_cmd)
  vim.cmd "packadd mini.nvim | helptags ALL"
  vim.cmd 'echo "Installed `mini.nvim`" | redraw'
end

require("mini.deps").setup { path = { package = path_package } }

local add = MiniDeps.add

-- Extendable (fuzzy) finding and search interface
add { source = "ibhagwan/fzf-lua" }

add { source = "stevearc/oil.nvim" }

add {
  source = "stevearc/conform.nvim",
}

add { source = "folke/flash.nvim" }

add { source = "tpope/vim-fugitive" }

add {
  source = "neovim/nvim-lspconfig",
  depends = { "folke/lazydev.nvim" },
}

add { source = "gpanders/nvim-parinfer" }

add { source = "actionshrimp/direnv.nvim" }

add { source = "olical/conjure" }

add { source = "nickel-lang/vim-nickel" }

add {
  source = "nvim-treesitter/nvim-treesitter",
  hooks = {
    post_checkout = function()
      vim.cmd "TSUpdate"
    end,
  },
}

add { source = "mbbill/undotree" }

add {
  source = "elixir-tools/elixir-tools.nvim",
  depends = { "nvim-lua/plenary.nvim" },
}

-- Themes
add { source = "miikanissi/modus-themes.nvim" }
add { source = "blazkowolf/gruber-darker.nvim" }
add { source = "neanias/everforest-nvim" }
add { source = "ellisonleao/gruvbox.nvim" }
add { source = "uhs-robert/oasis.nvim" }

add { source = "folke/noice.nvim", depends = { "MunifTanjim/nui.nvim" } }
