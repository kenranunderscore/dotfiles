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
    startupCommands = lib.mkOption { default = [ ]; };

    # TODO: type this
    workspaces = lib.mkOption {
      default = [{
        number = 1;
        label = "1";
      }];
    };
  };

  config = lib.mkIf cfg.enable {
    programs = {
      rofi = {
        enable = true;
        cycle = true;
        # font = "Pragmata Pro Mono 14";
        font = "Iosevka 15";
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
        startup = cfg.startupCommands;
        focus = {
          followMouse = true;
          mouseWarping = true;
        };
        menu = "rofi -disable-history -show run";
        window = {
          titlebar = false;
          border = 2;
        };
        keybindings = let
          workspaceKeybindings = builtins.foldl' (acc:
            { number, label }:
            let
              n = toString number;
              ws = "${n}:${label}";
            in acc // {
              "${modifier}+${n}" = "workspace ${ws}";
              "${modifier}+Shift+${n}" = "move container to workspace ${ws}";
            }) { } cfg.workspaces;
        in lib.mkOptionDefault ({
          # Disable the default workspaces
          "${modifier}+0" = null;
          "${modifier}+1" = null;
          "${modifier}+2" = null;
          "${modifier}+3" = null;
          "${modifier}+4" = null;
          "${modifier}+5" = null;
          "${modifier}+6" = null;
          "${modifier}+7" = null;
          "${modifier}+8" = null;
          "${modifier}+9" = null;
          "${modifier}+Shift+0" = null;
          "${modifier}+Shift+1" = null;
          "${modifier}+Shift+2" = null;
          "${modifier}+Shift+3" = null;
          "${modifier}+Shift+4" = null;
          "${modifier}+Shift+5" = null;
          "${modifier}+Shift+6" = null;
          "${modifier}+Shift+7" = null;
          "${modifier}+Shift+8" = null;
          "${modifier}+Shift+9" = null;
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
          "${modifier}+d" =
            lib.mkForce "exec rofi -disable-history -show-icons -show drun";
        } // workspaceKeybindings);
        bars = [ ];
        colors = {
          focused = {
            background = "#041a04";
            border = "#0ac30a";
            childBorder = "#00a000";
            indicator = "#00a000";
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
