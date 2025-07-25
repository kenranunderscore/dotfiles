{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.my.i3;
in
{
  options.my.i3 = {
    enable = lib.mkEnableOption "i3";

    terminal = lib.mkOption {
      type = lib.types.str;
      default = "${lib.getExe pkgs.kitty}";
    };

    # TODO: type this with hm.lib.options?
    startupCommands = lib.mkOption { default = [ ]; };

    # TODO: type this
    workspaces = lib.mkOption { default = [ ]; };

    withGaps = lib.mkOption {
      type = lib.types.bool;
      default = true;
    };
  };

  config = lib.mkIf cfg.enable {
    # FIXME: pull out the config generated from workspaces and merge.
    # Should make the code easier on the eyes.
    xsession.windowManager.i3 = {
      enable = true;
      package = if cfg.withGaps then pkgs.i3-gaps else pkgs.i3;
      config = rec {
        inherit (cfg) terminal;
        modifier = "Mod4";
        startup = cfg.startupCommands;
        focus = {
          followMouse = false;
          mouseWarping = true;
        };
        menu = "rofi -show run";
        window = {
          titlebar = false;
          border = 2;
        };
        assigns = builtins.foldl' (
          acc: w: if builtins.hasAttr "assigns" w then acc // { "${w.name}" = w.assigns; } else acc
        ) { } cfg.workspaces;
        bars = [ ];
        keybindings =
          let
            workspaceKeybindings = builtins.foldl' (
              acc: w:
              let
                n = builtins.head (builtins.split ":" w.name);
              in
              acc
              // {
                "${modifier}+${n}" = "workspace number ${w.name}";
                "${modifier}+Shift+${n}" = "move container to workspace number ${w.name}";
              }
            ) { } cfg.workspaces;
          in
          lib.mkOptionDefault (
            {
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
              "${modifier}+d" = lib.mkForce "exec rofi -disable-history -show-icons -show drun";
              "${modifier}+e" = "exec emacsclient -a '' --create-frame --no-wait";
            }
            // workspaceKeybindings
          );
        workspaceOutputAssign = builtins.map (w: {
          inherit (w) output;
          workspace = w.name;
        }) (builtins.filter (builtins.hasAttr "output") cfg.workspaces);
        colors = {
          focused = {
            background = "#041a04";
            border = "#b36d43";
            childBorder = "#b36d43";
            indicator = "#b36d43";
            text = "#b36d43";
          };
          focusedInactive = {
            background = "#5f676a";
            border = "#333333";
            childBorder = "#5f676a";
            indicator = "#484e50";
            text = "#eeeeee";
          };
          placeholder = {
            background = "#040504";
            border = "#040504";
            childBorder = "#0c0c0c";
            indicator = "#040504";
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
            background = "#040504";
            border = "#b36d43";
            childBorder = "#700000";
            indicator = "#700000";
            text = "#f03500";
          };
        };
        gaps =
          let
            val = 40;
          in
          lib.mkIf cfg.withGaps {
            inner = val;
            outer = 0;
            left = 0;
            right = 0;
            bottom = 0;
            top = 0;
            horizontal = 0;
            vertical = 0;
          };
      };
    };

    home.packages = [ pkgs.i3status ];
    xdg.configFile."i3status/config".source = ./i3status.config;
  };
}
