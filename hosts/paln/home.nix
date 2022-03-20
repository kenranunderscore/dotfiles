{ customConfig, config, lib, pkgs, ... }:

{
  imports = [ ../../modules ];

  config = {
    modules = {
      email = {
        enable = true;
        certificatesFile = "/etc/ssl/certs/ca-certificates.crt";
      };
      shell = {
        bash.enable = true;
        zsh.enable = true;
        pass = {
          enable = true;
          # FIXME customize GPG key in "user environment" module
          gpgKey = "0BAD1500D7D4282C433BC0BC9AC78C1A48681583";
        };
        tmux = {
          enable = true;
          shellPath = "${pkgs.zsh}/bin/zsh";
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

    home = rec {
      inherit (customConfig) username;
      homeDirectory = "/home/${username}";
      stateVersion = "21.03";
      packages = with pkgs; [ cacert curl fd gnumake ripgrep tree unzip wget ];
    };
  };
}
