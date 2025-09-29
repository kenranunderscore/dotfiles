require("elixir").setup {
  nextls = { enable = true },
  elixirls = {
    enable = true,
    settings = require("elixir.elixirls").settings {
      dialyzerEnabled = true,
      enableTestLenses = true,
      suggestSpecs = true,
    },
  },
}
