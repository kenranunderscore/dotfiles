{ config, lib, pkgs, ... }:

let cfg = config.modules.desktop.picom;
in {
  options.modules.desktop.picom = { enable = lib.mkEnableOption "picom"; };

  config = lib.mkIf cfg.enable {
    services.picom = {
      enable = true;
      activeOpacity = 0.95;
      inactiveOpacity = 0.7;
      opacityRules = [
        ''100:role = "browser"''
        ''100:class_i = "mattermost"''
        ''100:name *= "Thunderbird"''
        ''100:name *= "Minecraft"''
        ''100:class_i = "rofi"''
        ''100:class_i = "DBeaver"''
        ''100:class_i = "emacs"''
        ''100:class_i = "nyxt"''
        ''100:class_i = "VirtualBox Machine"''
        ''100:class_i = "VirtualBox Manager"''
        ''100:class_i = "urxvt"''
        ''100:class_i = "element"''
      ];
      backend = "glx";
      experimentalBackends = true;
      fade = false;
      fadeDelta = 5;
      vSync = true;
      settings = { xrender-sync-fence = true; };
    };
  };
}