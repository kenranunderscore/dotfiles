require("noice").setup {
  presets = {
    bottom_search = false,
    command_palette = {
      views = {
        cmdline_popup = {
          position = {
            row = "50%",
            col = "50%",
          },
        },
        cmdline_popupmenu = {
          position = {
            row = "67%",
            col = "50%",
          },
        },
      },
    },
    long_message_to_split = true,
  },
}
