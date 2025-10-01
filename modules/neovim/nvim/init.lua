vim.loader.enable()
require "config.options"
require "config.keymap"
require "config.deps"
require "config.snippets"

if vim.g.neovide then
  require "config.neovide"
end
