{ config, lib, pkgs, ... }:

let
  nixGLSource = let rev = "7d6bc1b21316bab6cf4a6520c2639a11c25a220e";
  in builtins.fetchTarball {
    url = "https://github.com/guibou/nixGL/archive/${rev}.tar.gz";
    sha256 = "02y38zmdplk7a9ihsxvnrzhhv7324mmf5g8hmxqizaid5k5ydpr3";
  };
  nixGL = (pkgs.callPackage "${nixGLSource}/nixGL.nix" { }).nixGLNvidia;
in rec {
  imports = [ ../base.nix ../../modules ];

  targets.genericLinux = { enable = true; };

  hosts.base = {
    username = "johannes";
    privateDir = ../../private/linux;
    shellPath = "${pkgs.fish}/bin/fish";
  };

  modules = {
    email = {
      certificatesFile = "/etc/ssl/certs/ca-certificates.crt";
      primaryAccount = "mailbox";
    };
    programs = {
      bspwm = {
        enable = true;
        configDir = ./bspwm;
      };
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
    shell = { git = { email = "johannes.maier@active-group.de"; }; };
  };

  programs = { rofi.enable = true; };

  services = {
    gpg-agent = {
      enable = true;
      enableSshSupport = true;
    };
  };

  home.packages = with pkgs; [
    feh
    htop
    kittyWrapped
    leiningen
    linphone
    manpages
    mercurial
    nextcloud-client
    subversion
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
