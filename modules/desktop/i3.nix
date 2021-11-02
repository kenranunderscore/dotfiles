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
        color_good = "#0ac90a";
        color_bad = "#ff4500";
        color_degraded = "#eec900";
      };
    };

    home.packages = [ pkgs.dmenu pkgs.j4-dmenu-desktop ];

    xsession.windowManager.i3 = let
      j4 = "${pkgs.j4-dmenu-desktop}/bin/j4-dmenu-desktop";
      dmenuOpts =
        "-nf \\#0AC90A -nb \\#040404 -sb \\#01018A -sf \\#0AC90A -b -l 3 -i -fn 'Iosevka-14'";
    in {
      enable = true;
      config = rec {
        inherit (cfg) terminal;
        modifier = "Mod4";
        startup = [{
          command = "setxkbmap -layout us -variant altgr-intl";
          always = true;
        }];
        defaultWorkspace = "workspace number 0";
        menu = "${j4} --dmenu='${pkgs.dmenu}/bin/dmenu_run ${dmenuOpts}'";
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
          "${modifier}+d" = lib.mkForce
            "exec ${j4} --dmenu='${pkgs.dmenu}/bin/dmenu ${dmenuOpts}'";
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
          colors = {
            background = "#040404";
            statusline = "#bbbbbb";
            separator = "#0ac80a";
            focusedWorkspace = {
              background = "#042004";
              border = "#0ac90a";
              text = "#0ac90a";
            };
            activeWorkspace = {
              background = "#040404";
              border = "#666666";
              text = "#0ac90a";
            };
            inactiveWorkspace = {
              background = "#040404";
              border = "#666666";
              text = "#909590";
            };
            bindingMode = {
              background = "#040404";
              border = "#ff4500";
              text = "#ff4500";
            };
            urgentWorkspace = {
              background = "#040404";
              border = "#ff4500";
              text = "#ff4500";
            };
          };
        }];
        colors = {
          focused = {
            background = "#042004";
            border = "#0ac90a";
            childBorder = "#007000";
            indicator = "#007000";
            text = "#0ac90a";
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
      };
    };
  };
}
