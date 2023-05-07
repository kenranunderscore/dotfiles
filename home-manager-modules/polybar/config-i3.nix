{ withBattery, font, pkgs }:

let
  colors = {
    foreground = "#0ac30a";
    background = "#040404";
    transparent = "#040404";
    urgent = "#ff4500";
  };
  mkLayoutTextModule = content: {
    inherit content;
    content-background = "${colors.transparent}";
    content-foreground = "${colors.background}";
    type = "custom/text";
    content-font = 2;
  };
  mkBar = bottom: modules-left: modules-center: modules-right: {
    inherit bottom modules-left modules-center modules-right;
    monitor = "\${env:MONITOR:}";
    background = "${colors.transparent}";
    foreground = "${colors.foreground}";
    fixed-center = true;
    font-0 = "${font.name}:size=15;4";
    font-1 = "${font.name}:style=Bold:size=15;4";
    # TODO: maybe Material is a better fit?
    font-2 = "Font Awesome 6 Free:size=17;5";
    font-3 = "Font Awesome 6 Free Solid:style=Solid:size=17;5";
    height = "30";
    locale = "en_US.UTF-8";
    offset-x = "0%";
    padding = "0";
    radius-top = "0";
    width = "100%";
  };
  mkWlanModule = interface: {
    inherit interface;
    type = "internal/network";
    accumulate-stats = "true";
    format-connected = " <label-connected>";
    format-connected-background = "${colors.background}";
    format-connected-foreground = "${colors.foreground}";
    format-connected-margin = 0;
    format-connected-overline = "${colors.transparent}";
    format-connected-padding = 2;
    format-connected-underline = "${colors.transparent}";
    format-disconnected = "<label-disconnected>";
    format-disconnected-background = "${colors.background}";
    format-disconnected-foreground = "#909090";
    format-disconnected-margin = 0;
    format-disconnected-overline = "${colors.transparent}";
    format-disconnected-padding = 2;
    format-disconnected-underline = "${colors.transparent}";
    interval = "1.0";
    label-connected = "%essid% %signal%%";
    label-disconnected = "DISCONNECTED";
    unknown-as-up = true;
  };
in {
  "global/wm" = {
    margin-bottom = 0;
    margin-top = 0;
  };

  "bar/main" = mkBar true "date distro-icon cpu memory" "xworkspaces"
    (pkgs.lib.optionalString withBattery "battery "
      + "wlan1 wlan2 wlan3 wlan4 audio powermenu");

  "module/xworkspaces" = {
    type = "internal/xworkspaces";
    enable-scroll = false;
    pin-workspaces = true;
    format = "<label-state>";
    label-monitor = "";
    label-active = "%name%";
    label-active-background = "${colors.background}";
    label-active-foreground = "${colors.foreground}";
    label-active-padding = 1;
    label-active-margin = 0;
    label-active-font = 2;
    label-occupied = "%name%";
    label-occupied-background = "${colors.background}";
    label-occupied-foreground = "#707070";
    label-occupied-padding = 1;
    label-occupied-margin = 0;
    label-empty = "";
    label-empty-padding = 0;
    label-empty-margin = 0;
    label-urgent = "%name%";
    label-urgent-background = "${colors.background}";
    label-urgent-foreground = "${colors.urgent}";
    label-urgent-padding = 1;
    label-urgent-margin = 0;
  };

  "module/date" = {
    format-foreground = "${colors.foreground}";
    format-background = "${colors.background}";
    format-padding = 2;
    format-margin = 0;
    interval = 1;
    label = " %date%   %time%";
    time = "%H:%M:%S";
    date = "%d %b %Y";
    type = "internal/date";
  };

  "module/cpu" = {
    # format = "%{T-2} %{T-}<label>";
    format = "%{T3} %{T-}<label>";
    format-background = "${colors.background}";
    format-foreground = "${colors.foreground}";
    format-padding = 2;
    interval = "0.5";
    label = "CPU %percentage%%";
    type = "internal/cpu";
  };

  "module/battery" = {
    type = "internal/battery";
    adapter = "AC0";
    animation-charging-0 = "";
    animation-charging-1 = "";
    animation-charging-2 = "";
    animation-charging-3 = "";
    animation-charging-4 = "";
    animation-charging-framerate = 500;
    battery = "BAT0";
    format-charging = " <animation-charging> <label-charging>";
    format-charging-background = "${colors.background}";
    format-charging-foreground = "${colors.foreground}";
    format-charging-padding = 1;
    format-discharging = "<ramp-capacity> <label-discharging>";
    format-discharging-background = "${colors.background}";
    format-discharging-foreground = "${colors.foreground}";
    format-discharging-padding = 1;
    format-full-background = "${colors.background}";
    format-full-foreground = "${colors.foreground}";
    format-full-padding = 1;
    full-at = 101;
    label-charging = "%percentage%%";
    label-discharging = "%percentage%%";
    label-full = " 100%";
    poll-interval = 2;
    ramp-capacity-0 = " ";
    ramp-capacity-0-foreground = "#e74c3c";
    ramp-capacity-1 = " ";
    ramp-capacity-1-foreground = "#e74c3c";
    ramp-capacity-2 = " ";
    ramp-capacity-3 = " ";
    ramp-capacity-4 = " ";
  };

  "module/audio" = {
    format-muted = "<label-muted>";
    format-muted-background = "${colors.background}";
    format-muted-foreground = "${colors.urgent}";
    format-muted-overline = "${colors.transparent}";
    format-muted-padding = 2;
    format-muted-margin = 0;
    format-muted-prefix = "%{T4} %{T-}";
    format-muted-prefix-foreground = "${colors.urgent}";
    format-volume = "%{T4} %{T-}VOL <label-volume>";
    format-volume-background = "${colors.background}";
    format-volume-foreground = "${colors.foreground}";
    format-volume-padding = 2;
    format-volume-margin = 0;
    label-muted = "MUTED";
    label-volume = "%percentage%%";
    type = "internal/pulseaudio";
  };

  "settings" = {
    compositing-background = "source";
    compositing-border = "over";
    compositing-foreground = "over";
    compositing-overline = "over";
    comppositing-underline = "over";
    pseudo-transparency = false;
    screenchange-reload = true;
    throttle-output = "5";
    throttle-output-for = "10";
  };

  "module/powermenu" = {
    type = "custom/menu";
    expand-left = true;
    format = "<label-toggle> <menu>";
    format-background = "${colors.background}";
    format-foreground = "${colors.foreground}";
    format-padding = 1;
    label-close = " ";
    label-close-padding-right = 0;
    label-close-padding-left = 1;
    label-open = " ";
    label-open-padding = 1;
    label-separator = "|";
    label-separator-padding = 1;
    menu-0-0 = "  Suspend";
    menu-0-0-exec = "systemctl suspend";
    menu-0-1 = "  Reboot";
    menu-0-1-exec = "v";
    menu-0-2 = "  Shutdown";
    menu-0-2-exec = "systemctl poweroff";
  };

  "module/wlan1" = mkWlanModule "wlp3s0";
  "module/wlan2" = mkWlanModule "wlp4s0";
  "module/wlan3" = mkWlanModule "wlp1s0";
  "module/wlan4" = mkWlanModule "wlp0s20f3";

  "module/memory" = {
    format = "%{T4} %{T-}<label>";
    format-background = "${colors.background}";
    format-foreground = "${colors.foreground}";
    format-padding = 2;
    format-margin = 0;
    interval = 3;
    label = "RAM %percentage_used%%";
    type = "internal/memory";
  };

  "module/distro-icon" = {
    exec = "${pkgs.coreutils}/bin/uname -r | ${pkgs.coreutils}/bin/cut -d- -f1";
    format = "%{T3} %{T-}<label>";
    format-background = "${colors.background}";
    format-foreground = "${colors.foreground}";
    format-padding = 2;
    interval = "999999999";
    label = "%output%";
    type = "custom/script";
  };
}
