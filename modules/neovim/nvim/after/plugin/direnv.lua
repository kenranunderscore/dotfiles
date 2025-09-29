require("direnv-nvim").setup {
  async = true,
  on_direnv_finished = function()
    vim.cmd "LspStart"
  end,
}
