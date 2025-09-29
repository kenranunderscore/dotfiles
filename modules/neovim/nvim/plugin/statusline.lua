require("mini.statusline").setup {
  content = {
    active = function()
      local mode, mode_hl = MiniStatusline.section_mode { trunc_width = 120 }
      local git = MiniStatusline.section_git { trunc_width = 40 }
      local diagnostics = MiniStatusline.section_diagnostics { trunc_width = 75 }
      local lsp = MiniStatusline.section_lsp { trunc_width = 75 }
      local filename = MiniStatusline.section_filename { trunc_width = 140 }
      local fileinfo = MiniStatusline.section_fileinfo { trunc_width = 100 }
      local search = MiniStatusline.section_searchcount { trunc_width = 75 }
      local location = MiniStatusline.section_location { trunc_width = 75 }

      return MiniStatusline.combine_groups {
        { hl = mode_hl, strings = { string.upper(mode) } },
        { hl = "MiniStatuslineDevinfo", strings = { git, diagnostics, lsp } },
        "%<",
        { hl = "MiniStatuslineFilename", strings = { filename } },
        "%=",
        { hl = "MiniStatuslineFileinfo", strings = { fileinfo } },
        { hl = mode_hl, strings = { search, location } },
      }
    end,
  },
}
