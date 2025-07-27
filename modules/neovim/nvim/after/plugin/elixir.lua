local elixir = require "elixir"
local elixirls = require "elixir.elixirls"

elixir.setup {
  nextls = { enable = true },
  elixirls = {
    enable = true,
    settings = elixirls.settings {
      dialyzerEnabled = true,
      enableTestLenses = true,
      suggestSpecs = true,
    },
  },
}
