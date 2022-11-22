require("kenran.set")
require("kenran.plugins")
require("kenran.general_keymap")
require("kenran.format")

-- work around .exrc not loading when using init.lua instead of init.vim
-- TODO: switch to vim.secure.read(), once I know how to source the result
if vim.loop.fs_stat(".nvimrc") then
    vim.cmd('sandbox source .nvimrc')
elseif vim.loop.fs_stat(".exrc") then
    vim.cmd('sandbox source .exrc')
end
