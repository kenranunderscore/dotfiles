return {
    "stevearc/oil.nvim",
    dependencies = { { "echasnovski/mini.icons" } },
    lazy = false,
    keys = {
        { "<leader>dd", "<cmd>Oil<cr>", desc = "open directory browser" },
    },
    config = function()
        require("oil").setup {
            keymaps = {
                ["<C-s>"] = false,
            }
        }
    end
}
