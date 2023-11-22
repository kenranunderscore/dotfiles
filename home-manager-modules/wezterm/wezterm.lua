local wezterm = require 'wezterm'

local config = {}

config.enable_tab_bar = false
config.color_scheme = 'Everforest Dark (Gogh)'
config.font = wezterm.font 'Iosevka Comfy'
config.font_size = 17.0
config.audible_bell = "Disabled"
config.default_cursor_style = "SteadyUnderline"
config.underline_thickness = "0.08cell"

return config
