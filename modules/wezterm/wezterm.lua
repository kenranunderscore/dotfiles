local wezterm = require 'wezterm'

local config = {}

config.enable_tab_bar = false
config.font = wezterm.font 'Hack'
config.font_size = 14.0
config.audible_bell = "Disabled"

-- Cursor
config.default_cursor_style = "SteadyUnderline"
config.cursor_thickness = 2
config.cursor_blink_rate = 600

config.default_prog = { 'fish', '-l' }

config.colors = {
  foreground = "#0ac30a",
  background = "#060606",
  selection_fg = "#0eb40e",
  selection_bg = "#05058a",
  cursor_bg = "#60c410",
  ansi = {
    "#707370",
    "#ff9000",
    "#b3ee3a",
    "#eec900",
    "#00bfb0",
    "#cc59d2",
    "#00bfb0",
    "#d5d5d5",
  },
  brights = {
    "#545454",
    "#d99000",
    "#83bc10",
    "#b89c00",
    "#00afa0",
    "#825c84",
    "#00afa0",
    "#e5e5e5",
  }
}

return config
