{ inputs, config, lib, pkgs, ... }:

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
  pragmataPro =
    pkgs.callPackage (import ./pp.nix "${inputs.privateConfig}/linux") { };
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
      iosevkaSerif
      pragmataPro
      pkgs.anonymousPro
      pkgs.camingo-code
      pkgs.cantarell-fonts
      pkgs.cascadia-code
      pkgs.courier-prime
      pkgs.fira-code
      pkgs.font-awesome_5
      pkgs.go-font
      pkgs.hack-font
      pkgs.hasklig
      pkgs.ibm-plex
      pkgs.inconsolata
      (import ./input-mono.nix { inherit pkgs; })
      pkgs.iosevka
      pkgs.jetbrains-mono
      pkgs.liberation_ttf
      pkgs.lmodern
      pkgs.nerdfonts
      pkgs.noto-fonts
      pkgs.roboto-mono
      pkgs.source-code-pro
      pkgs.terminus_font
      pkgs.ubuntu_font_family
    ];
  };
}
