# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  nixpkgs.config.allowUnfree = true;

  imports = [ # Include the results of the hardware scan.
    ./hardware-configuration.nix
  ];

  # Use the systemd-boot EFI boot loader.
  boot = {
    kernelPackages = pkgs.linuxPackages_latest;
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;
  };

  networking = {
    hostName = "atuan";
    networkmanager.enable = true;
    useDHCP = false;
    interfaces = {
      enp2s0.useDHCP = true;
      wlp3s0.useDHCP = true;
    };
  };

  time.timeZone = "Europe/Berlin";

  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };
  i18n.defaultLocale = "en_US.UTF-8";

  services = {
    printing = {
      enable = true;
      drivers = [ pkgs.samsungUnifiedLinuxDriver ];
    };
    xserver = {
      enable = true;
      videoDrivers = [ "nvidia" ];
      libinput = {
        enable = true;
        naturalScrolling = true;
      };
      displayManager = {
        defaultSession = "none+bspwm";
        lightdm.enable = true;
        lightdm.greeters.mini.enable = true;
        lightdm.greeters.mini.user = "kenran";
      };
      windowManager.bspwm.enable = true;
    };
  };

  programs.ssh.startAgent = true;

  documentation = {
    enable = true;
    man.enable = true;
    dev.enable = true;
  };

  sound.enable = true;
  hardware = {
    pulseaudio.enable = true;
    opengl = {
      enable = true;
      driSupport32Bit = true;
    };
  };

  users.users.kenran = {
    isNormalUser = true;
    home = "/home/kenran";
    extraGroups = [ "wheel" "networkmanager" "docker" ];
  };

  nix.trustedUsers = [ "root" "kenran" ];

  environment.systemPackages = with pkgs; [ ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.03"; # Did you read the comment?

}
