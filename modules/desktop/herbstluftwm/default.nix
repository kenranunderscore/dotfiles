{ config, lib, pkgs, ... }:

let cfg = config.modules.desktop.herbstluftwm;
in {
  options.modules.desktop.herbstluftwm.enable =
    lib.mkEnableOption "herbstluftwm";

  config = lib.mkIf cfg.enable {
    xsession.windowManager.herbstluftwm = let
      mod = "Mod4";
      prependMod = lib.mapAttrs' (key: lib.nameValuePair "${mod}-${key}");
      resizeStep = "0.05";
    in rec {
      enable = true;
      settings = {
        window_border_width = 0;
        window_border_active_color = "#0033aa";
      };
      # Add real tags manually later.
      tags = [ "1" "2" "3" "4" "5" "6" "7" "8" "9" ];
      keybinds = prependMod {
        o = "split right 0.5";
        Shift-o = "split left 0.5";
        u = "split bottom 0.5";
        Shift-u = "split top 0.5";
        l = "focus right";
        h = "focus left";
        k = "focus up";
        j = "focus down";
        Shift-l = "shift right";
        Shift-h = "shift left";
        Shift-k = "shift up";
        Shift-j = "shift down";
        Control-l = "resize right ${resizeStep}";
        Control-h = "resize left ${resizeStep}";
        Control-k = "resize up ${resizeStep}";
        Control-j = "resize down ${resizeStep}";
        t = "spawn kitty";
        space = "spawn rofi -disable-history -show run";
        r = "remove";
        s = "floating toggle";
        f = "fullscreen toggle";
        p = "pseudotile toggle";
      };
      mousebinds = prependMod {
        B1 = "move";
        B2 = "zoom";
        B3 = "resize";
      };
      extraConfig = ''
        for i in ${lib.escapeShellArgs tags}; do
          if ! [ -z "$i" ]; then
            index=$(expr $i - 1)
            herbstclient keybind "${mod}-$i" use_index "$index"
            herbstclient keybind "${mod}-Shift-$i" move_index "$index"
          fi
        done
        herbstclient use_index 0

        xset r rate 200 55
        ~/.fehbg
      '';
    };
  };
}
