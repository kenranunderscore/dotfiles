vim.loader.enable()
require "config.options"
require "config.keymap"
require "config.deps"

if vim.g.neovide then
  require "config.neovide"
end
