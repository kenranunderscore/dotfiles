{
  config,
  pkgs,
  custom,
  ...
}:

let
  username = custom.username;
in
{
  imports = [ ./hardware-configuration.nix ];

  boot = {
    loader.grub.enable = true;
    loader.grub.device = "/dev/sda";
    tmp.cleanOnBoot = true;
  };

  networking.hostName = "paln";

  time.timeZone = "Europe/Amsterdam";

  networking.useDHCP = false;
  networking.interfaces.ens3.useDHCP = true;

  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  users.users.${username} = {
    isNormalUser = true;
    home = "/home/${username}";
    extraGroups = [ "wheel" ];
  };
  nix.settings.trusted-users = [
    "root"
    username
  ];

  environment = {
    systemPackages = with pkgs; [ vim ];
    pathsToLink = [ "/share/zsh" ];
  };

  system.activationScripts = {
    create_var_www = "mkdir -m 0777 -p /var/www/kenran.info";
  };

  security.acme = rec {
    acceptTerms = true;
    defaults = {
      email = "johannes.maier@mailbox.org";
      renewInterval = "daily";
    };
    certs."kenran.info".email = defaults.email;
  };

  services = {
    openssh.enable = true;
    syncthing = {
      enable = true;
      user = username;
      dataDir = "/home/${username}/sync";
      configDir = "/home/${username}/.config/syncthing";
    };

    nginx = {
      enable = true;
      virtualHosts."kenran.info" = {
        forceSSL = true;
        enableACME = true;
        root = "/var/www/kenran.info";
      };
    };
  };

  programs.ssh.startAgent = true;

  networking.firewall = {
    enable = true;
    allowedTCPPorts = [
      22000
      80
      443
    ];
    allowedUDPPorts = [
      21027
      22000
    ];
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.09"; # Did you read the comment?
}
