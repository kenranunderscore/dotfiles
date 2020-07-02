# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  nixpkgs.config.allowUnfree = true;

  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "roke";
  networking.useDHCP = false;
  networking.interfaces.enp3s0.useDHCP = true;

  console.font = "Lat2-Terminus16";
  console.keyMap = "us";
  i18n = {
    defaultLocale = "en_US.UTF-8";
  };

  time.timeZone = "Europe/Berlin";

  environment.systemPackages = with pkgs; [
  ];

  # Some fonts I like
  fonts.fonts = with pkgs; [
    ibm-plex
    source-code-pro
  ];

  programs.ssh.startAgent = true;

  sound.enable = true;
  hardware.pulseaudio.enable = true;

  services.xserver = {
    enable = true;
    libinput.naturalScrolling = true;
    libinput.enable = true;
    displayManager = {
      defaultSession = "none+bspwm";
      lightdm.enable = true;
      lightdm.greeters.mini.enable = true;
      lightdm.greeters.mini.user = "kenran";
    };
    windowManager.bspwm.enable = true;
  };

  users.users.kenran = {
    isNormalUser = true;
    home = "/home/kenran";
    extraGroups = [ "wheel" "networkmanager" ];
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "20.09"; # Did you read the comment?
}

