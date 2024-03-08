{ custom, config, lib, pkgs, ... }:

let
  cfg = config.modules.kitty;
  types = lib.types;
in {
  options.modules.kitty = { enable = lib.mkEnableOption "kitty"; };

  config = lib.mkIf cfg.enable {
    programs.kitty = {
      enable = true;
      font = { inherit (custom.font) name size; };
      keybindings = {
        "ctrl+shift+equal" = "change_font_size all +1.0";
        "ctrl+shift+plus" = "change_font_size all +1.0";
        "ctrl+shift+minus" = "change_font_size all -1.0";
      };
      shellIntegration.mode = "disabled";
      settings = {
        term = "xterm-256color";
        macos_option_as_alt = true;
        disable_ligatures = "always";
        adjust_line_height = 1;
        scrollback_lines = 50000;
        hide_window_decorations = false;
        remember_window_size = false;
        initial_window_width = 800;
        initial_window_height = 520;
        enable_audio_bell = false;
        window_padding_width = "0";

        # Set custom color scheme that is used nearly everywhere:
        # Dimmed Naga colors
        background = "#040504";
        foreground = "#0eb40e";
        cursor = "#f01500";
        cursor_text_color = "background";
        cursor_shape = "block";
        cursor_blink_interval = "-1";
        cursor_stop_blinking_after = "0";
        selection_background = "#01073a";
        selection_foreground = "#0eb40e";
        # black
        color0 = "#707370";
        color8 = "#545454";
        # red
        color1 = "#d99000";
        color9 = "#d99000";
        # green
        color2 = "#83bc10";
        color10 = "#83bc10";
        # yellow
        color3 = "#b89c00";
        color11 = "#b89c00";
        # blue
        color4 = "#00afa0";
        color12 = "#00afa0";
        # magenta
        color5 = "#825c84";
        color13 = "#825c84";
        # cyan
        color6 = "#00afa0";
        color14 = "#00afa0";
        # white
        color7 = "#e5e5e5";
        color15 = "#d5d5d5";
      };
    };
  };
}
