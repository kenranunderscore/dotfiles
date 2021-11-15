{ config, lib, pkgs, ... }:

let
  cfg = config.modules.fonts;
  iosevkaSerif = pkgs.iosevka.override {
    privateBuildPlan = {
      family = "Iosevka Custom";
      spacing = "normal";
      serifs = "slab";
      ligations.inherits = "haskell";
      slopes = {
        upright = {
          angle = 0;
          shape = "upright";
          menu = "upright";
          css = "normal";
        };
        italic = {
          angle = 9.4;
          shape = "oblique";
          menu = "italic";
          css = "italic";
        };
        oblique = {
          angle = 9.4;
          shape = "oblique";
          menu = "oblique";
          css = "oblique";
        };
      };
    };
    set = "iosevka-custom";
  };
in {
  options.modules.fonts = {
    enable = lib.mkEnableOption "fonts";

    withCustomBuilds = lib.mkOption {
      type = lib.types.bool;
      default = true;
    };
  };

  config = lib.mkIf cfg.enable {
    fonts.fontconfig.enable = lib.mkForce true;

    home.packages = [
      pkgs.anonymousPro
      pkgs.camingo-code
      pkgs.cantarell_fonts
      pkgs.cascadia-code
      pkgs.fira-code
      pkgs.go-font
      pkgs.hack-font
      pkgs.hasklig
      pkgs.hermit
      pkgs.ibm-plex
      pkgs.inconsolata
      pkgs.iosevka
      iosevkaSerif
      pkgs.jetbrains-mono
      pkgs.meslo-lg
      pkgs.roboto-mono
      pkgs.source-code-pro
      pkgs.terminus_font
      pkgs.ubuntu_font_family
      pkgs.unifont
      pkgs.uw-ttyp0
    ];
  };
}
