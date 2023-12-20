{ inputs, custom, config, lib, pkgs, ... }:

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

    home = {
      sessionVariables.KENRAN_DEFAULT_FONT = custom.font.name;

      packages = lib.optionals cfg.withCustomBuilds [
        (import ./sf-mono.nix {
          inherit (pkgs) runCommand;
          inherit (inputs) sf-mono;
        })
        (pkgs.callPackage ./lucida-console.nix { })
        pragmataPro
        (import ./termingus.nix { inherit (pkgs) runCommand fetchFromGitHub; })
        (pkgs.callPackage (import ./oldschool.nix) { })
        (pkgs.callPackage (import ./twilio-sans-mono.nix) {
          inherit (pkgs) runCommand unzip;
          inherit (inputs) twilio-sans-mono;
        })
      ] ++ (with pkgs; [
        borg-sans-mono
        cantarell-fonts
        cascadia-code
        corefonts
        courier-prime
        fantasque-sans-mono
        fira-code
        font-awesome_6
        go-font
        hack-font
        hasklig
        ibm-plex
        inconsolata
        iosevka-bin
        jetbrains-mono
        julia-mono
        noto-fonts
        roboto-mono
        source-code-pro
        unifont
        victor-mono
        vistafonts
      ]);
    };
  };
}
