{ config, lib, pkgs, ... }:

let cfg = config.modules.desktop.sway;
in {
  options.modules.desktop.sway = {
    enable = lib.mkEnableOption "sway";
    terminal = lib.mkOption {
      type = lib.types.str;
      default = "${pkgs.foot}/bin/foot";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.font-awesome ];

    wayland.windowManager.sway = let
      j4 = "${pkgs.j4-dmenu-desktop}/bin/j4-dmenu-desktop";
      dmenuOpts =
        "-nf \\#0AC90A -nb \\#040404 -sb \\#01018A -sf \\#0AC90A -b -l 3 -i -fn 'Iosevka-14'";
    in {
      enable = true;
      wrapperFeatures.gtk = true;
      systemdIntegration = true;
      config = rec {
        inherit (cfg) terminal;
        modifier = "Mod4";
        input = {
          "type:keyboard" = {
            xkb_layout = "us";
            xkb_variant = "altgr-intl";
          };
          "type:pointer" = {
            natural_scroll = "enabled";
            accel_profile = "adaptive";
          };
          "type:touchpad" = { tap = "enabled"; };
        };
        output = {
          HDMI-A-1 = { pos = "0 0"; };
          eDP-1 = {
            pos = "3840 0";
            res = "1920x1080@72.007Hz";
          };
        };
        menu = "${j4} --dmenu='${pkgs.dmenu}/bin/dmenu_run ${dmenuOpts}'";
        startup = [{
          command = "systemctl --user restart waybar";
          always = true;
        }];
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
        bars = [ ];
      };
    };

    programs = {
      waybar = {
        enable = true;
        systemd = {
          enable = true;
          target = "sway-session.target";
        };
        style = ./waybarStyle.css;
        settings = [{
          layer = "bottom";
          position = "bottom";
          output = [ "eDP-1" "HDMI-A-1" ];
          modules-left = [ "sway/workspaces" "sway/mode" "custom/rarrow" ];
          modules-center = [ ];
          modules-right = [
            "custom/larrow"
            "pulseaudio"
            "custom/larrow-2"
            "custom/larrow"
            "network"
            "custom/larrow-2"
            "custom/larrow"
            "cpu"
            "custom/larrow-2"
            "custom/larrow"
            "memory"
            "custom/larrow-2"
            "custom/larrow"
            "temperature"
            "custom/larrow-2"
            "custom/larrow"
            "battery"
            "custom/larrow-2"
            "custom/larrow"
            "clock#date"
            "custom/larrow-2"
            "custom/larrow"
            "clock#time"
          ];
          "custom/larrow" = {
            format = "";
            tooltip = false;
          };
          "custom/larrow-2" = {
            format = "";
            tooltip = false;
          };
          "custom/rarrow" = {
            format = "";
            tooltip = false;
          };
          "sway/workspaces" = {
            disable-scroll = true;
            all-outputs = false;
          };
          "clock#date" = {
            interval = 60;
            format = " {:%e %b %Y}";
            tooltip = false;
          };
          "clock#time" = {
            interval = 1;
            format = "{:%H:%M:%S}";
            tooltip = false;
          };
          pulseaudio = {
            format = "{icon} {volume}%  {format_source}";
            format-bluetooth = "{volume}% {icon}  {format_source}";
            format-bluetooth-muted = " {icon}  {format_source}";
            format-muted = " {format_source}";
            format-source = " {volume}%";
            format-source-muted = "";
            format-icons = {
              headphone = "";
              hands-free = "";
              headset = "";
              phone = "";
              portable = "";
              car = "";
              default = [ "" "" "" ];
            };
            tooltip = false;
          };
          network = {
            format-wifi = " {ifname} ({signalStrength}%)";
            format-ethernet = " {ipaddr}/{cidr}";
            tooltip-format = " {ifname} via {gwaddr}";
            format-linked = " {ifname} (No IP)";
            format-disconnected = "⚠ Disconnected";
            format-alt = "{essid}: {ipaddr}/{cidr}";
            tooltip = false;
          };
          cpu = {
            format = " {load} ({usage}%)";
            tooltip = false;
          };
          memory = {
            format = " {used:0.1f}G {}%";
            tooltip = false;
          };
          temperature = {
            interval = 5;
            critical-threshold = 80;
            format = "{icon} {temperatureC}°C";
            format-icons = [ "" "" "" "" ];
            tooltip = true;
          };
          battery = {
            format = "{icon} {capacity}%";
            states = {
              good = 90;
              warning = 50;
              critical = 15;
            };
            format-icons = [ "" "" "" "" "" ];
            tooltip = false;
          };
        }];
      };
    };
  };
}
