return {
  "nvim-lualine/lualine.nvim",
  lazy = false,
  opts = {
    options = {
      icons_enabled = true,
      theme = "auto",
      component_separators = { left = "", right = "" },
      section_separators = { left = "", right = "" },
    },
    sections = {
      lualine_a = { "mode" },
      lualine_b = { "filename" },
      lualine_c = { "location" },
      lualine_x = { "encoding", "fileformat" },
      lualine_y = {},
      lualine_z = { "filetype" },
    },
    inactive_sections = {
      lualine_a = {},
      lualine_b = { "filename" },
      lualine_c = { "location" },
      lualine_x = { "encoding", "fileformat" },
      lualine_y = {},
      lualine_z = { "filetype" },
    },
  },
}
