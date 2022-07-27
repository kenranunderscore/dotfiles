{ inputs, config, lib, pkgs, ... }:

let
  cfg = config.modules.fonts;
  pragmataPro =
    pkgs.callPackage (import ./pp.nix "${inputs.privateConfig}") { };
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

    home.packages = lib.optionals cfg.withCustomBuilds [
      (import ./input-mono.nix { inherit pkgs; })
      (import ./sf-mono.nix {
        inherit (pkgs) runCommand;
        inherit (inputs) sf-mono;
      })
      pragmataPro
    ] ++ [
      pkgs.anonymousPro
      pkgs.camingo-code
      pkgs.cantarell-fonts
      pkgs.cascadia-code
      pkgs.courier-prime
      pkgs.fantasque-sans-mono
      pkgs.fira-code
      pkgs.font-awesome_5
      pkgs.go-font
      pkgs.hack-font
      pkgs.hasklig
      pkgs.ibm-plex
      pkgs.inconsolata
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
