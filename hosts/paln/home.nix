{ config, lib, pkgs, ... }:

let dag = import <home-manager/modules/lib/dag.nix> { inherit lib; };
in {
  imports = [ ../../modules ];

  config = {
    nixpkgs = { config = import ../../nix/nixpkgs-config.nix; };
    modules = {
      email = {
        enable = true;
        certificatesFile = "/etc/ssl/certs/ca-certificates.crt";
        isSyncServer = true;
      };
      shell = {
        bash.enable = true;
        fish.enable = true;
        git.enable = true;
        pass = {
          enable = true;
          # FIXME customize GPG key in "user environment" module
          gpgKey = "0BAD1500D7D4282C433BC0BC9AC78C1A48681583";
        };
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
      ssh.enable = true;
    };

    services = {
      gpg-agent = {
        enable = true;
        enableSshSupport = true;
      };
    };

    home = {
      username = "kenran";
      homeDirectory = "/home/kenran";
      stateVersion = "21.03";
      packages = with pkgs; [ cacert curl fd gnumake ripgrep tree unzip wget ];

      activation = let privateDir = ../../private/linux;
      in {
        handlePrivateKeys = let privateKeyPath = privateDir + "/id_rsa";
        in dag.dagEntryAfter [ "writeBoundary" ] ''
          $DRY_RUN_CMD ln -sf ${
            builtins.toPath privateKeyPath
          } $HOME/.ssh/id_rsa && \
          $DRY_RUN_CMD cd ${builtins.toPath ../../.}/private && \
          $DRY_RUN_CMD chmod 400 *.pem **/*.key **/id_rsa* && \
          $DRY_RUN_CMD ssh-add $HOME/.ssh/id_rsa && \
          $DRY_RUN_CMD gpg --import ${privateDir + "/gpg.key"}
        '';
      };
    };

    xdg.configFile = {
      "nixpkgs/config.nix".source = ../../nix/nixpkgs-config.nix;
    };
  };
}
