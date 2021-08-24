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
        color_good = "#4c963e";
        color_bad = "#993e4c";
        color_degraded = "#3e4c96";
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
        menu = "${pkgs.dmenu}/bin/dmenu_run -b -l 5 -fn 'Terminus-14'";
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
          "${modifier}+s" = "split h";
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
            names = [ "Terminus" ];
            size = 14.0;
          };
        }];
      };
    };
  };
}
