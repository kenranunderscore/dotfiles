{ config, lib, pkgs, ... }:

let
  nixGLSource = let rev = "7d6bc1b21316bab6cf4a6520c2639a11c25a220e";
  in builtins.fetchTarball {
    url = "https://github.com/guibou/nixGL/archive/${rev}.tar.gz";
    sha256 = "02y38zmdplk7a9ihsxvnrzhhv7324mmf5g8hmxqizaid5k5ydpr3";
  };
  nixGL = (pkgs.callPackage "${nixGLSource}/nixGL.nix" { }).nixGLNvidia;
in rec {
  imports = [ ./base.nix ../modules ];

  targets.genericLinux = { enable = true; };

  hosts.base = {
    username = "johannes";
    privateDir = ../private/linux;
    shellPath = "${pkgs.fish}/bin/fish";
  };

  modules = {
    email = {
      certificatesFile = "/etc/ssl/certs/ca-certificates.crt";
      primaryAccount = "mailbox";
    };
    programs = {
      kitty = {
        enable = true;
        useLoginShell = false;
        fontSize = "10.0";
      };
      qutebrowser = {
        package = pkgs.writeShellScriptBin "qb" ''
          #!/usr/bin/env sh
          ${nixGL}/bin/nixGLNvidia ${pkgs.qutebrowser}/bin/qutebrowser "$@"
        '';
      };
    };
    shell = {
      git = {
        email = "johb.maier@gmail.com";
        gpgKey = "0BAD1500D7D4282C433BC0BC9AC78C1A48681583";
      };
    };
  };

  programs = { rofi.enable = true; };

  services = {
    gpg-agent = {
      enable = true;
      enableSshSupport = true;
    };
  };

  home.packages = with pkgs; [
    htop
    kittyWrapped
    manpages
    nextcloud-client
    polybar
    sxhkd
    xorg.xkbcomp
  ];

  # wrap kitty with nixGL, since it does not expose a 'package' option
  nixpkgs.overlays = [
    (_: super: {
      kittyWrapped = pkgs.writeShellScriptBin "ky" ''
        #!/usr/bin/env sh
        ${nixGL}/bin/nixGLNvidia ${super.kitty}/bin/kitty "$@"
      '';
    })
  ];
}
