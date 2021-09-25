{ config, lib, pkgs, ... }:

let cfg = config.modules.desktop.i3;
in {
  options.modules.desktop.i3 = {
    enable = lib.mkEnableOption "i3";

    terminal = lib.mkOption {
      type = lib.types.str;
      default = "${pkgs.kitty}/bin/kitty";
    };
  };

  config = {
    programs.i3status = {
      enable = true;
      enableDefault = true;
      general = {
        colors = true;
        color_good = "#005000";
        color_bad = "#700000";
        color_degraded = "#202060";
      };
    };

    home.packages = [ pkgs.dmenu ];

    xsession.windowManager.i3 = {
      enable = true;
      config = rec {
        inherit (cfg) terminal;
        modifier = "Mod4";
        startup = [{
          command = "setxkbmap -layout us -variant altgr-intl";
          always = true;
        }];
        defaultWorkspace = "workspace number 0";
        menu = "${pkgs.dmenu}/bin/dmenu_run -b -l 5 -fn 'Hack-13'";
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
        };
        bars = [{
          position = "bottom";
          mode = "dock";
          statusCommand = "${pkgs.i3status}/bin/i3status";
          trayOutput = null;
          workspaceButtons = true;
          workspaceNumbers = true;
          hiddenState = "hide";
          fonts = {
            names = [ "Hack" ];
            size = 13.0;
          };
        }];
        colors = {
          focused = {
            background = "#285577";
            border = "#4c7899";
            childBorder = "#007000";
            indicator = "#007000";
            text = "#ffffff";
          };
        };
      };
    };
  };
}
