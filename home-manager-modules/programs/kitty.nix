{ custom, config, lib, pkgs, ... }:

let
  cfg = config.modules.programs.kitty;
  types = lib.types;
in {
  options.modules.programs.kitty = { enable = lib.mkEnableOption "kitty"; };

  config = lib.mkIf cfg.enable {
    programs.kitty = {
      enable = true;
      font = { inherit (custom.font) name size; };
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
        # Set custom color scheme that is used nearly everywhere.
        background = "#040404";
        foreground = "#0ac30a";
        cursor = "#f00000";
        selection_background = "#a4c5ef";
        color0 = "#707370";
        color8 = "#545454";
        color1 = "#ff4500";
        color9 = "#ff4500";
        color2 = "#eec900";
        color10 = "#eec900";
        color3 = "#b3ee3a";
        color11 = "#b3ee3a";
        color4 = "#00cdcd";
        color12 = "#00cdcd";
        color5 = "#cc59d2";
        color13 = "#cc59d2";
        color6 = "#00cdcd";
        color14 = "#00cdcd";
        color7 = "#f4f4f4";
        color15 = "#e5e5e5";
        selection_foreground = "#000000";
      };
    };
  };
}
