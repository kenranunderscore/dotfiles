{ config, lib, ... }:

let
  cfg = config.my.picom;
in
{
  options.my.picom = {
    enable = lib.mkEnableOption "picom";
  };

  config = lib.mkIf cfg.enable {
    services.picom = {
      enable = true;
      activeOpacity = 1.0;
      inactiveOpacity = 1.0;
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
        ''100:class_i = "DrRacket"''
        ''100:class_i = ".drracket-wrapped"''
        ''100:class_i = "code"''
        ''100:class_i = "Alacritty"''
      ];
      backend = "glx";
      fade = true;
      fadeDelta = 5;
      vSync = true;
      settings = {
        xrender-sync-fence = true;
        corner-radius = 0;
      };
    };
  };
}
