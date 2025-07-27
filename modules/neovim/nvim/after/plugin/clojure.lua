local cmp = require "cmp"
local config = cmp.get_config()
table.insert(config.sources, { name = "conjure" })
cmp.setup(config)
