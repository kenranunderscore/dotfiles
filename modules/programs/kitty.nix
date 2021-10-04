{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.programs.kitty;
in {
  options.modules.programs.kitty = {
    enable = mkEnableOption "kitty";

    shellPath = mkOption {
      type = types.str;
      default = "${pkgs.bash}/bin/bash";
    };

    useLoginShell = mkOption {
      type = types.bool;
      default = false;
    };

    fontSize = mkOption {
      # TODO make it int/float
      type = types.str;
      default = "12.0";
    };
  };

  config = mkIf cfg.enable {
    programs.kitty = {
      enable = true;
      font = {
        package = pkgs.fira-code;
        name = "Fira Code";
      };
      settings = {
        term = "xterm-256color";
        shell = cfg.shellPath + (if cfg.useLoginShell then " --login" else "");
        macos_option_as_alt = true;
        font_size = cfg.fontSize;
        adjust_line_height = 0;
        scrollback_lines = 50000;
        hide_window_decorations = false;
        remember_window_size = false;
        initial_window_width = 800;
        initial_window_height = 520;
        enable_audio_bell = false;
        window_padding_width = "0 10 0 10";
        # Color scheme VibrantInk from dexpota/kitty-themes
        background = "#040404";
        foreground = "#0ac30a";
        cursor = "#f00000";
        selection_background = "#a4c5ef";
        color0 = "#707370";
        color8 = "#545454";
        color1 = "#dc7612";
        color9 = "#ff0000";
        color2 = "#eec900";
        color10 = "#00ff00";
        color3 = "#b3ee3a";
        color11 = "#ffff00";
        color4 = "#00cdcd";
        color12 = "#0000ff";
        color5 = "#f474b4";
        color13 = "#ff00ff";
        color6 = "#00cdcd";
        color14 = "#00ffff";
        color7 = "#f4f4f4";
        color15 = "#e5e5e5";
        selection_foreground = "#000000";
      };
    };
  };
}
