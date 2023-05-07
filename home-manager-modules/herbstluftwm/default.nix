{ inputs, config, lib, pkgs, ... }:

let cfg = config.modules.herbstluftwm;
in {
  options.modules.herbstluftwm.enable = lib.mkEnableOption "herbstluftwm";

  config = lib.mkIf cfg.enable {
    xsession.windowManager.herbstluftwm = let
      mod = "Mod4";
      withModKey = lib.mapAttrs' (key: lib.nameValuePair "${mod}-${key}");
    in {
      enable = true;
      settings = { window_border_width = 0; };
      tags = [ "1" "2" "3" "4" "5" ];
      keybinds = withModKey {
        t = "spawn kitty";
        space = "spawn rofi -disable-history -show run";
        r = "remove";
        f = "fullscreen toggle";
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
        Control-l = "resize right 0.05";
        Control-h = "resize left 0.05";
        Control-k = "resize up 0.05";
        Control-j = "resize down 0.05";
      };
      mousebinds = withModKey {
        B1 = "move";
        B2 = "zoom";
        B3 = "resize";
      };
      extraConfig = ''
        for i in $(seq 1 9); do
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
