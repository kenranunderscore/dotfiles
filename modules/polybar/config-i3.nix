{
  withBattery,
  font,
  pkgs,
}:

let
  colors = {
    foreground = "#78824b";
    accent = "#c9a554";
    background = "#222222";
    highlight-background = "#2c2f2c";
    transparent = "#222222";
    urgent = "#685742";
  };
  mkBar = bottom: modules-left: modules-center: modules-right: {
    inherit
      bottom
      modules-left
      modules-center
      modules-right
      ;
    monitor = "\${env:MONITOR:}";
    background = "${colors.transparent}";
    foreground = "${colors.foreground}";
    fixed-center = true;
    font-0 = "${font.name}:size=18;3";
    font-1 = "${font.name}:style=Bold:size=18;3";
    # TODO: maybe Material is a better fit?
    font-2 = "Font Awesome 6 Free:size=17;5";
    font-3 = "Font Awesome 6 Free Solid:style=Solid:size=17;5";
    height = "33";
    locale = "en_US.UTF-8";
    offset-x = "0%";
    padding = "0";
    radius-top = "0";
    width = "100%";
    separator = "|";
    line-size = "5";
  };
  mkWlanModule = interface: {
    inherit interface;
    type = "internal/network";
    accumulate-stats = "true";
    format-connected-prefix = " ";
    format-connected-prefix-foreground = "${colors.accent}";
    format-connected = "<label-connected>";
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
in
{
  "global/wm" = {
    margin-bottom = 0;
    margin-top = 0;
  };

  "bar/main" = mkBar true "i3" "" (
    "distro-icon cpu memory"
    + (pkgs.lib.optionalString withBattery " battery " + "wlan1 wlan2 wlan3 wlan4 audio date")
  );

  "module/date" = {
    format-prefix = " ";
    format-prefix-foreground = "${colors.accent}";
    format-foreground = "${colors.foreground}";
    format-background = "${colors.background}";
    format-padding = 2;
    format-margin = 0;
    interval = 1;
    label = "%date% %time%";
    time = "%H:%M:%S";
    date = "%d %b %Y";
    type = "internal/date";
  };

  "module/cpu" = {
    format = "<label>";
    format-prefix = "%{T3} %{T-}";
    format-prefix-foreground = "${colors.accent}";
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
    format-charging-prefix = " ";
    format-charging-prefix-foreground = "${colors.accent}";
    format-charging = "<animation-charging> <label-charging>";
    format-charging-background = "${colors.background}";
    format-charging-foreground = "${colors.foreground}";
    format-charging-padding = 1;
    format-discharging = "<ramp-capacity> <label-discharging>";
    format-discharging-background = "${colors.background}";
    format-discharging-foreground = "${colors.foreground}";
    format-discharging-padding = 1;
    format-full-prefix = " ";
    format-full-prefix-foreground = "${colors.accent}";
    format-full-background = "${colors.background}";
    format-full-foreground = "${colors.foreground}";
    format-full-padding = 1;
    full-at = 99;
    label-charging = "%percentage%%";
    label-discharging = "%percentage%%";
    label-full = "100%";
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
    format-volume = "<label-volume>";
    format-volume-prefix = "%{T4} %{T-}";
    format-volume-prefix-foreground = "${colors.accent}";
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
    compositing-underline = "over";
    pseudo-transparency = false;
    screenchange-reload = true;
    throttle-output = "5";
    throttle-output-for = "10";
  };

  "module/wlan1" = mkWlanModule "wlp3s0";
  "module/wlan2" = mkWlanModule "wlp4s0";
  "module/wlan3" = mkWlanModule "wlp1s0";
  "module/wlan4" = mkWlanModule "wlp0s20f3";

  "module/memory" = {
    format = "<label>";
    format-prefix = "%{T4} %{T-}";
    format-prefix-foreground = "${colors.accent}";
    format-background = "${colors.background}";
    format-foreground = "${colors.foreground}";
    format-padding = 2;
    format-margin = 0;
    interval = 3;
    label = "RAM %percentage_used%%";
    type = "internal/memory";
  };

  "module/i3" = {
    type = "internal/i3";
    pin-workspaces = true;
    strip-wsnumbers = true;
    index-sort = true;
    enable-click = true;
    enable-scroll = false;
    format = "<label-state><label-mode>";
    label-separator = "|";
    label-separator-padding = 0;
    label-separator-margin = 0;
    label-mode = "%mode%";
    label-mode-background = "${colors.background}";
    label-mode-foreground = "${colors.urgent}";
    label-mode-padding = 1;
    label-mode-margin = 0;
    label-focused = "%name%";
    label-focused-background = "${colors.highlight-background}";
    label-focused-foreground = "${colors.accent}";
    label-focused-underline = "${colors.accent}";
    label-focused-padding = 1;
    label-focused-margin = 0;
    label-focused-font = 2;
    label-unfocused = "%name%";
    label-unfocused-background = "${colors.background}";
    label-unfocused-foreground = "${colors.foreground}";
    label-unfocused-padding = 1;
    label-unfocused-margin = 0;
    label-visible = "%name%";
    label-visible-background = "${colors.highlight-background}";
    label-visible-foreground = "${colors.accent}";
    label-visible-padding = 1;
    label-visible-margin = 0;
    label-urgent = "%name%";
    label-urgent-background = "${colors.background}";
    label-urgent-foreground = "${colors.urgent}";
    label-urgent-padding = 1;
    label-urgent-margin = 0;
  };

  "module/distro-icon" = {
    exec = "${pkgs.coreutils}/bin/uname -r | ${pkgs.coreutils}/bin/cut -d- -f1";
    format-prefix = "%{T3} %{T-}";
    format-prefix-foreground = "${colors.accent}";
    format = "<label>";
    format-background = "${colors.background}";
    format-foreground = "${colors.foreground}";
    format-padding = 2;
    interval = "999999999";
    label = "%output%";
    type = "custom/script";
  };
}
