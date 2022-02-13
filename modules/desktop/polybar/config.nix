pkgs:

let
  mkLayoutTextModule = content: {
    inherit content;
    content-background = "#00000000";
    content-foreground = "#040404";
    type = "custom/text";
    content-font = 2;
  };
  mkBar = bottom: modules-left: modules-center: modules-right: {
    inherit bottom modules-left modules-center modules-right;
    monitor = "\${env:MONITOR:}";
    background = "#00000000";
    foreground = "#0ac30a";
    fixed-center = true;
    font-0 = "FuraCode Nerd Font:size=15;3";
    font-1 = "FuraCode Nerd Font:style=Bold:size=19;4";
    font-2 = "FuraCode Nerd Font:style=Bold:size=15;3";
    height = "30";
    locale = "en_US.UTF-8";
    offset-x = "1%";
    padding = "0";
    radius-top = "0";
    width = "100%";
  };
  mkWlanModule = interface: {
    inherit interface;
    type = "internal/network";
    accumulate-stats = "true";
    format-connected = "<label-connected>";
    format-connected-background = "#040404";
    format-connected-foreground = "#0ac30a";
    format-connected-margin = 0;
    format-connected-overline = "#00000000";
    format-connected-padding = 2;
    format-connected-underline = "#00000000";
    format-disconnected = "<label-disconnected>";
    format-disconnected-background = "#040404";
    format-disconnected-foreground = "#909090";
    format-disconnected-margin = 0;
    format-disconnected-overline = "#00000000";
    format-disconnected-padding = 2;
    format-disconnected-underline = "#00000000";
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

  "bar/bottom" = mkBar true "cpu tri1 tri2 memory tri1" "title"
    "tri3 wlan1 wlan2 tri4 tri3 powermenu";

  "bar/top" =
    mkBar false "distro-icon tri4" "tri2 i3 tri4" "tri2 audio tri1 tri2 date";

  "module/tri1" = mkLayoutTextModule "";
  "module/tri2" = mkLayoutTextModule "";
  "module/tri3" = mkLayoutTextModule "";
  "module/tri4" = mkLayoutTextModule " ";

  "module/date" = {
    format = "<label>";
    format-foreground = "#0ac30a";
    format-background = "#040404";
    format-padding = 2;
    format-margin = 0;
    interval = 1;
    label = "%date% %time%";
    time = "%H:%M:%S";
    date = "%d %b %Y";
    type = "internal/date";
  };

  "module/cpu" = {
    format = " <label>";
    format-background = "#040404";
    format-foreground = "#0ac30a";
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
    format-charging-background = "#91ddff";
    format-charging-foreground = "#141228";
    format-charging-padding = 1;
    format-discharging = "<ramp-capacity> <label-discharging>";
    format-discharging-background = "#91ddff";
    format-discharging-foreground = "#141228";
    format-discharging-padding = 1;
    format-full-background = "#91ddff";
    format-full-foreground = "#141228";
    format-full-padding = 1;
    full-at = 101;
    label-charging = "%percentage%% +%consumption%W";
    label-discharging = "%percentage%% -%consumption%W";
    label-full = " 100%";
    poll-interval = 2;
    ramp-capacity-0 = "";
    ramp-capacity-0-foreground = "#e74c3c";
    ramp-capacity-1 = "";
    ramp-capacity-1-foreground = "#e74c3c";
    ramp-capacity-2 = "";
    ramp-capacity-3 = "";
    ramp-capacity-4 = "";
  };

  "module/audio" = {
    format-muted = "<label-muted>";
    format-muted-background = "#040404";
    format-muted-foreground = "#ff4500";
    format-muted-overline = "#00000000";
    format-muted-padding = 2;
    format-muted-margin = 0;
    format-muted-prefix = "婢 ";
    format-muted-prefix-foreground = "#ff4500";
    format-volume = "墳  VOL <label-volume>";
    format-volume-background = "#040404";
    format-volume-foreground = "#0ac30a";
    format-volume-padding = 2;
    format-volume-margin = 0;
    label-muted = "MUTED";
    label-volume = "%percentage%%";
    type = "internal/pulseaudio";
  };

  "module/i3" = {
    type = "internal/i3";
    pin-workspaces = true;
    enable-click = true;
    enable-scroll = false;
    format = "<label-state><label-mode>";
    label-separator = "";
    label-separator-padding = 0;
    label-separator-margin = 0;
    label-mode = "%mode%";
    label-mode-background = "#040404";
    label-mode-foreground = "#ff4500";
    label-mode-padding = 1;
    label-mode-margin = 0;
    label-focused = "%index%";
    label-focused-background = "#040404";
    label-focused-foreground = "#0ac30a";
    label-focused-padding = 1;
    label-focused-margin = 0;
    label-focused-font = 3;
    label-unfocused = "%index%";
    label-unfocused-background = "#040404";
    label-unfocused-foreground = "#707070";
    label-unfocused-padding = 1;
    label-unfocused-margin = 0;
    label-visible = "%index%";
    label-visible-background = "#040404";
    label-visible-foreground = "#707070";
    label-visible-padding = 1;
    label-visible-margin = 0;
    label-urgent = "%index%";
    label-urgent-background = "#040404";
    label-urgent-foreground = "#ff4500";
    label-urgent-padding = 1;
    label-urgent-margin = 0;
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

  "module/title" = {
    type = "internal/xwindow";
    label = "%title%";
    label-maxlen = "70";
    format = "<label>";
    format-foreground = "cccccc";
  };

  "module/powermenu" = {
    type = "custom/menu";
    expand-left = true;
    format = "<label-toggle> <menu>";
    format-background = "#040404";
    format-foreground = "#0ac30a";
    format-padding = 1;
    label-close = " ";
    label-close-padding-right = 0;
    label-close-padding-left = 1;
    label-open = "";
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

  "module/memory" = {
    format = " <label>";
    format-background = "#040404";
    format-foreground = "#0ac30a";
    format-padding = 2;
    format-margin = 0;
    interval = 3;
    label = "RAM %percentage_used%%";
    type = "internal/memory";
  };

  "module/distro-icon" = {
    exec = "${pkgs.coreutils}/bin/uname -r | ${pkgs.coreutils}/bin/cut -d- -f1";
    format = "   <label>";
    format-background = "#040404";
    format-foreground = "#0ac30a";
    format-padding = 2;
    interval = "999999999";
    label = "%output%";
    type = "custom/script";
  };
}
