{ config, lib, pkgs, ... }:

let cfg = config.modules.desktop.picom;
in {
  options.modules.desktop.picom = { enable = lib.mkEnableOption "picom"; };

  config = lib.mkIf cfg.enable {
    services.picom = {
      enable = true;
      activeOpacity = "0.95";
      inactiveOpacity = "0.7";
      opacityRule = [
        ''100:role = "browser"''
        ''100:class_i = "mattermost"''
        ''100:name *= "Thunderbird"''
        ''100:class_i = "rofi"''
      ];
      backend = "glx";
      experimentalBackends = true;
      fade = true;
      fadeDelta = 5;
      vSync = true;
      extraOptions = "xrender-sync-fence = true;";
    };
  };
}
