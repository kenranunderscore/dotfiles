{ config, lib, pkgs, ... }:

let dag = import <home-manager/modules/lib/dag.nix> { inherit lib; };
in {
  imports = [ ../base.nix ../../modules ];

  hosts.base = {
    username = "maier";
    homeDirectory = "/Users/maier";
    privateDir = ../../private/macos;
    shellPath = "${pkgs.fish}/bin/fish";
  };

  modules = {
    email = {
      certificatesFile = "/usr/local/etc/openssl/cert.pem";
      primaryAccount = "ag";
    };
    programs = {
      emacs = { version = lib.mkForce "stable"; };
      kitty = {
        useLoginShell = true;
        fontSize = "17.0";
      };
    };
    shell = {
      git = {
        email = "johannes.maier@active-group.de";
        gpgKey = "0x4DC80C3B727DC1EE";
      };
    };
  };

  home = {
    activation = {
      symlinkMacOSApps = let
        action =
          "$DRY_RUN_CMD ln -snf $HOME/.nix-profile/Applications/*.app ~/Applications/";
      in dag.dagEntryAfter [ "writeBoundary" ] action;
    };
  };
}
