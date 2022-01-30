{ config, pkgs, ... }:

{
  imports = [ ./hardware-configuration.nix ];

  nixpkgs.config.allowUnfree = true;
  nix = {
    package = pkgs.nixUnstable;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
    trustedUsers = [ "root" "johannes" ];
  };

  boot.loader = {
    systemd-boot.enable = true;
    grub = {
      enable = true;
      default = "saved";
      useOSProber = true;
      efiSupport = true;
      device = "nodev";
    };
    efi.canTouchEfiVariables = true;
  };

  time.timeZone = "Europe/Berlin";

  networking = {
    useDHCP = false;
    interfaces.enp3s0f1.useDHCP = true;
    interfaces.wlp4s0.useDHCP = true;
    hostName = "zangief";
    networkmanager.enable = true;
  };

  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  services = {
    xserver = {
      enable = true;
      videoDrivers = [ "nvidia" ];
      dpi = 96;
      layout = "us";
      libinput = {
        enable = true;
        mouse.naturalScrolling = true;
        touchpad.naturalScrolling = true;
      };
      layout = "us";
      xkbVariant = "altgr-intl";
      displayManager.startx.enable = true;
    };
    printing.enable = true;
  };

  documentation = {
    enable = true;
    man.enable = true;
    dev.enable = true;
  };

  sound.enable = true;
  hardware = {
    nvidia.modesetting.enable = false;
    pulseaudio.enable = true;
    opengl.enable = true;
  };

  users.users.johannes = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" "docker" ];
  };

  programs.ssh.startAgent = true;

  virtualisation.docker.enable = true;

  fileSystems."/ubuntu" = {
    device = "/dev/sda2";
    fsType = "ext4";
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.05"; # Did you read the comment?
}
