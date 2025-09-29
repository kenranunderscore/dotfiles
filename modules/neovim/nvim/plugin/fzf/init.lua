local fzf = require "fzf-lua"
fzf.setup {
  "ivy",
  fzf_opts = {
    -- Override some values that are otherwise inherited from
    -- my shell's $FZF_DEFAULT_OPTS
    ["--no-select-1"] = true,
    ["--no-exit-0"] = true,
    ["--layout"] = "reverse",
  },
  winopts = {
    height = 0.2,
    preview = {
      winopts = {
        number = false,
        relativenumber = false,
      },
    },
  },
  files = {
    previewer = false,
  },
  oldfiles = {
    previewer = false,
  },
  buffers = {
    previewer = false,
  },
  grep = {
    winopts = {
      height = 0.7,
      preview = {
        horizontal = "right:40%",
      },
    },
  },
  helptags = {
    previewer = false,
  },
}

fzf.register_ui_select()
