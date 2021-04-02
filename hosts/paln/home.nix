{ config, lib, pkgs, ... }:

{
  imports = [ ../../modules ];

  config = {
    nixpkgs = { config = import ../../nix/nixpkgs-config.nix; };
    modules = {
      email = {
        enable = true;
        certificatesFile = "/etc/ssl/certs/ca-certificates.crt";
      };
      shell = {
        bash.enable = true;
        fish.enable = true;
        git.enable = true;
        tmux = {
          enable = true;
          shellPath = "${pkgs.fish}/bin/fish";
        };
      };
    };

    programs = {
      gpg.enable = true;
      home-manager.enable = true;
      mbsync.enable = true;
      msmtp.enable = true;
      password-store.enable = true;
      ssh.enable = true;
    };

    home = {
      username = "kenran";
      homeDirectory = "/home/kenran";
      stateVersion = "21.03";
      packages = with pkgs; [ cacert curl fd gnumake ripgrep tree unzip wget ];
    };

    xdg.configFile = {
      "nixpkgs/config.nix".source = ../../nix/nixpkgs-config.nix;
    };
  };
}
