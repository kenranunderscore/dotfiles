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
        background = "#000000";
        foreground = "#ffffff";
        cursor = "#ffffff";
        selection_background = "#b4d5ff";
        color0 = "#868686";
        color8 = "#545454";
        color1 = "#ff6600";
        color9 = "#ff0000";
        color2 = "#ccff04";
        color10 = "#00ff00";
        color3 = "#ffcc00";
        color11 = "#ffff00";
        color4 = "#44b3cc";
        color12 = "#0000ff";
        color5 = "#9933cc";
        color13 = "#ff00ff";
        color6 = "#44b3cc";
        color14 = "#00ffff";
        color7 = "#f4f4f4";
        color15 = "#e5e5e5";
        selection_foreground = "#000000";
      };
    };
  };
}
