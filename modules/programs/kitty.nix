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
        package = pkgs.hack-font;
        name = "Hack";
      };
      settings = {
        term = "xterm-256color";
        shell = cfg.shellPath + (if cfg.useLoginShell then " --login" else "");
        macos_option_as_alt = true;
        font_size = cfg.fontSize;
        adjust_line_height = 1;
        scrollback_lines = 50000;
        hide_window_decorations = false;
        remember_window_size = false;
        initial_window_width = 800;
        initial_window_height = 520;
        enable_audio_bell = false;
        window_padding_width = "0 10 0 10";
        # Color scheme
        background = "#191919";
        foreground = "#776b53";
        cursor = "#fac814";
        selection_background = "#776b53";
        color0 = "#321200";
        color8 = "#423625";
        color1 = "#b1270e";
        color9 = "#ed5c20";
        color2 = "#44a900";
        color10 = "#55f237";
        color3 = "#a9810b";
        color11 = "#f1b731";
        color4 = "#578499";
        color12 = "#85cfec";
        color5 = "#96363c";
        color13 = "#e04b5a";
        color6 = "#b2591d";
        color14 = "#f07c14";
        color7 = "#776b53";
        color15 = "#ffc800";
        selection_foreground = "#191919";
      };
    };
  };
}
