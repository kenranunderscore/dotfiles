{ config, lib, pkgs, ... }:

let cfg = config.modules.desktop.i3;
in {
  options.modules.desktop.i3 = {
    enable = lib.mkEnableOption "i3";

    terminal = lib.mkOption {
      type = lib.types.str;
      default = "${pkgs.kitty}/bin/kitty";
    };

    # TODO: type this with hm.lib.options?
    additionalStartupCommands = lib.mkOption { default = [ ]; };
  };

  config = lib.mkIf cfg.enable {
    programs = {
      rofi = {
        enable = true;
        cycle = true;
        font = "Pragmata Pro Mono 14";
        location = "center";
        terminal = "${pkgs.kitty}/bin/kitty";
        theme = ./rofi/kenran.rasi;
        extraConfig = { modi = "run,drun,ssh,window"; };
      };
    };

    xsession.windowManager.i3 = {
      enable = true;
      package = pkgs.i3-gaps;
      config = rec {
        inherit (cfg) terminal;
        modifier = "Mod4";
        startup = let
          cmds = if config.modules.desktop.polybar.enable then [
            {
              command = "${../polybar/launch.sh}";
              always = false;
              notification = false;
            }
            {
              command = "~/.fehbg";
              always = false;
              notification = false;
            }
          ] else
            [ ];
        in cmds ++ cfg.additionalStartupCommands;
        defaultWorkspace = "workspace number 1";
        menu = "rofi -disable-history -show run";
        window = {
          titlebar = false;
          border = 2;
        };
        keybindings = lib.mkOptionDefault {
          # Use normal vim keys for moving between windows.
          "${modifier}+h" = "focus left";
          "${modifier}+l" = "focus right";
          "${modifier}+j" = "focus down";
          "${modifier}+k" = "focus up";
          "${modifier}+Shift+h" = "move left";
          "${modifier}+Shift+l" = "move right";
          "${modifier}+Shift+j" = "move down";
          "${modifier}+Shift+k" = "move up";
          "${modifier}+v" = "split v";
          "${modifier}+g" = "split h";
          "${modifier}+t" = "exec ${terminal}";
          "${modifier}+space" = lib.mkForce "exec ${menu}";
          "${modifier}+d" = lib.mkForce "exec rofi -disable-history -show drun";
        };
        bars = [ ];
        colors = {
          focused = {
            background = "#041a04";
            border = "#0ac30a";
            childBorder = "#007000";
            indicator = "#007000";
            text = "#0ac30a";
          };
          focusedInactive = {
            background = "#5f676a";
            border = "#333333";
            childBorder = "#5f676a";
            indicator = "#484e50";
            text = "#eeeeee";
          };
          placeholder = {
            background = "#040404";
            border = "#040404";
            childBorder = "#0c0c0c";
            indicator = "#040404";
            text = "#eeeeee";
          };
          unfocused = {
            background = "#222222";
            border = "#333333";
            childBorder = "#222222";
            indicator = "#292d2e";
            text = "#909590";
          };
          urgent = {
            background = "#040404";
            border = "#ff4500";
            childBorder = "#700000";
            indicator = "#700000";
            text = "#ff4500";
          };
        };
        gaps = let val = 20;
        in {
          inner = val;
          outer = val;
          left = 0;
          right = 0;
          bottom = 0;
          top = 0;
          horizontal = val;
          vertical = val;
        };
      };
    };
  };
}
