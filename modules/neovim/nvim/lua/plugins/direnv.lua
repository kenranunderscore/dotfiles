return {
  "actionshrimp/direnv.nvim",
  event = "VeryLazy",
  opts = {
    async = true,
    on_direnv_finished = function()
      vim.cmd "LspStart"
    end,
  },
}
