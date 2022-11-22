if vim.loop.fs_stat('.nvimrc') then
    vim.cmd("sandbox source .nvimrc")
elseif vim.loop.fs_stat('.exrc') then
    vim.cmd("sandbox source .exrc")
end
