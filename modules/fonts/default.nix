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
      (import ./sf-mono.nix {
        inherit (pkgs) runCommand;
        inherit (inputs) sf-mono;
      })
      (import ./lucida-console.nix { inherit (pkgs) runCommand fetchurl; })
      pragmataPro
    ] ++ (with pkgs; [
      anonymousPro
      borg-sans-mono
      camingo-code
      cantarell-fonts
      cascadia-code
      courier-prime
      fantasque-sans-mono
      fira-code
      font-awesome_5
      go-font
      gohufont
      hack-font
      hasklig
      ibm-plex
      inconsolata
      iosevka
      jetbrains-mono
      liberation_ttf
      lmodern
      nerdfonts
      noto-fonts
      roboto-mono
      source-code-pro
      terminus_font
      ubuntu_font_family
      vistafonts
    ]);
  };
}
