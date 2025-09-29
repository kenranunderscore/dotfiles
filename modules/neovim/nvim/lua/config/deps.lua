-- Clone 'mini.nvim' manually in a way that it gets managed by 'mini.deps'
local path_package = vim.fn.stdpath "data" .. "/site/"
local mini_path = path_package .. "pack/deps/start/mini.nvim"
if not vim.loop.fs_stat(mini_path) then
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

add {
  source = "ibhagwan/fzf-lua",
  -- TODO: mini.icons
  depends = { "nvim-tree/nvim-web-devicons" },
}

add {
  source = "stevearc/oil.nvim",
  depends = { "nvim-mini/mini.icons" },
}

add {
  source = "stevearc/conform.nvim",
}

add { source = "folke/flash.nvim" }

add { source = "folke/flash.nvim" }

add { source = "tpope/vim-fugitive" }

add {
  source = "neovim/nvim-lspconfig",
  depends = { "nvim-mini/mini.nvim", "folke/lazydev.nvim" },
}

add { source = "gpanders/nvim-parinfer" }

add { source = "actionshrimp/direnv.nvim" }

add { source = "olical/conjure" }

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
add { source = "olimorris/onedarkpro.nvim" }
add { source = "shaunsingh/solarized.nvim" }
add { source = "eldritch-theme/eldritch.nvim" }
add { source = "ptdewey/darkearth-nvim" }
add { source = "bluz71/vim-moonfly-colors" }

add { source = "folke/noice.nvim", depends = { "MunifTanjim/nui.nvim" } }
