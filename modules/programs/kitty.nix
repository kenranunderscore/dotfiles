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
        background = "#001014";
        foreground = "#405555";
        cursor = "#49fcd5";
        selection_background = "#415554";
        color0 = "#012026";
        color8 = "#374350";
        color1 = "#b12f2c";
        color9 = "#ff4242";
        color2 = "#00a940";
        color10 = "#2aea5e";
        color3 = "#5d8aa9";
        color11 = "#8dd3fd";
        color4 = "#449985";
        color12 = "#61d4b9";
        color5 = "#00599c";
        color13 = "#1298ff";
        color6 = "#5c7e19";
        color14 = "#98cf28";
        color7 = "#405554";
        color15 = "#58fad6";
        selection_foreground = "#001014";
      };
    };
  };
}
