local snippets = require "mini.snippets"
local gen_loader = snippets.gen_loader
snippets.setup {
  snippets = {
    gen_loader.from_file "~/.config/nvim/snippets/global.lua",
    gen_loader.from_lang(),
  },
}
snippets.start_lsp_server()
