vim.loader.enable()
require "config.options"
require "config.keymap"
require "config.lazy"

if vim.g.neovide then
  require "config.neovide"
end
