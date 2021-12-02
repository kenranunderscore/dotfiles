{ config, pkgs, ... }:

{
  nix = {
    package = pkgs.nixUnstable;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };
  nixpkgs.config.allowUnfree = true;

  imports = [ ./hardware-configuration.nix ];

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
    syncthing = {
      enable = true;
      user = "kenran";
      dataDir = "/home/kenran/sync";
      configDir = "/home/kenran/.config/syncthing";
    };
    xserver = {
      enable = true;
      videoDrivers = [ "nvidia" ];
      libinput = {
        enable = true;
        mouse.naturalScrolling = true;
      };
      displayManager = {
        session = [{
          manage = "window";
          name = "fake";
          start = "";
        }];
        defaultSession = "none+fake";
        lightdm = {
          enable = true;
          greeters.mini = {
            enable = true;
            user = "kenran";
          };
        };
        autoLogin = {
          user = "kenran";
          enable = true;
        };
        job.logToJournal = true;
      };
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

  virtualisation.docker.enable = true;

  networking.firewall = {
    enable = true;
    allowedTCPPorts = [ 22000 ];
    allowedUDPPorts = [ 22000 ];
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.05"; # Did you read the comment?
}
