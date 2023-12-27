{ custom, config, lib, pkgs, ... }:

{
  imports = [ ../../home-manager-modules ];

  config = {
    modules = {
      bash.enable = true;
      email = {
        enable = true;
        certificatesFile = "/etc/ssl/certs/ca-certificates.crt";
      };
      zsh.enable = true;
      pass = {
        enable = true;
        # FIXME customize GPG key in "user environment" module
        gpgKey = "0BAD1500D7D4282C433BC0BC9AC78C1A48681583";
      };
      tmux.enable = true;
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
        pinentryFlavor = "curses";
      };
    };

    home = rec {
      inherit (custom) username;
      homeDirectory = "/home/${username}";
      stateVersion = "21.03";
      packages = with pkgs; [ cacert curl fd gnumake ripgrep tree unzip wget ];
    };
  };
}
