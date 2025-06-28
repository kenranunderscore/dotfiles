local function git_files_with_fallback()
  vim.fn.system("git rev-parse --is-inside-work-tree")
  if vim.v.shell_error == 0 then
    require("telescope.builtin").git_files({})
  else
    require("telescope.builtin").find_files({})
  end
end

vim.keymap.set("n", "<leader>pf", git_files_with_fallback)
