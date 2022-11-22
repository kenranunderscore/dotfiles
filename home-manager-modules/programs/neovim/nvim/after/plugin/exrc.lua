local contents = nil
if vim.loop.fs_stat('.nvimrc') then
    contents = vim.secure.read('.nvimrc')
elseif vim.loop.fs_stat('.exrc') then
    contents = vim.secure.read('.exrc')
end

if contents then
    vim.api.nvim_exec(contents, nil)
end
