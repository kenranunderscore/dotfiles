{ config, pkgs, custom, ... }:

let username = custom.username;
in {
  imports = [ ./hardware-configuration.nix ];

  nixpkgs.config.allowUnfree = true;
  nix = {
    package = pkgs.nixUnstable;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
    settings.trusted-users = [ "root" username ];
  };

  boot = {
    kernelPackages = pkgs.linuxPackages_latest;
    kernelPatches = [ ];
    loader = {
      grub = {
        enable = true;
        configurationLimit = 20;
        default = "saved";
        useOSProber = true;
        efiSupport = true;
        device = "nodev";
      };
      efi.canTouchEfiVariables = true;
    };
  };

  time.timeZone = "Europe/Berlin";

  networking = {
    useDHCP = false;
    interfaces.enp0s31f6.useDHCP = true;
    interfaces.wlp0s20f3.useDHCP = true;
    hostName = "tehanu";
    networkmanager.enable = true;
    firewall = {
      allowedTCPPorts = [ 25565 ];
      allowedUDPPorts = [ 25565 ];
    };
  };

  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  fonts.fontconfig.allowBitmaps = true;

  services = {
    udev.packages = [ pkgs.qmk-udev-rules ];
    openssh = {
      enable = true;
      ports = [ 22 ];
    };
    xserver = {
      enable = true;
      videoDrivers = [ "nvidia" ];
      dpi = 96;
      libinput = {
        enable = true;
        mouse.naturalScrolling = true;
        touchpad.naturalScrolling = true;
      };
      layout = "us";
      xkbVariant = "altgr-intl";
      displayManager = {
        session = [{
          manage = "window";
          name = "fake";
          start = "";
        }];
        defaultSession = "none+fake";
        lightdm = {
          enable = true;
          greeters.slick.enable = true;
        };
      };
    };
    pipewire = {
      enable = true;
      pulse.enable = true;
      alsa.enable = false;
    };
    printing.enable = true;
    avahi = {
      enable = true;
      nssmdns = true;
      openFirewall = true;
    };
  };

  documentation = {
    enable = true;
    man.enable = true;
    dev.enable = true;
  };

  hardware = {
    nvidia.prime = {
      sync.enable = true;
      intelBusId = "PCI:0:2:0";
      nvidiaBusId = "PCI:1:0:0";
    };
    opengl = {
      enable = true;
      driSupport32Bit = true;
    };
  };

  users.users.${username} = {
    uid = 1000;
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" "docker" ];
    shell = pkgs.fish;
  };
  users.extraGroups.vboxusers.members = [ username ];

  programs = {
    ssh.startAgent = true;
    fish.enable = true;
  };

  virtualisation = { docker.enable = true; };

  environment = {
    systemPackages = with pkgs; [ git ];
    pathsToLink = [ "/share/zsh" ];
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.05"; # Did you read the comment?
}
