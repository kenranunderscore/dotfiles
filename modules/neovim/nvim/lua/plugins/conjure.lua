return {
  { "olical/conjure", ft = { "clojure", "fennel" }, lazy = true, dependencies = { "PaterJason/cmp-conjure" } },
  {
    "PaterJason/cmp-conjure",
    lazy = true,
    config = function()
      local cmp = require("cmp")
      local config = cmp.get_config()
      table.insert(config.sources, { name = "conjure" })
      return cmp.setup(config)
    end,
  },
}
