{ config, pkgs, custom, ... }:

let username = custom.username;
in {
  nix = {
    package = pkgs.nixUnstable;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
    settings.trusted-users = [ "root" username ];
  };

  imports = [ ./hardware-configuration.nix ];

  boot = {
    kernelPackages = pkgs.linuxPackages_latest;
    loader = {
      efi.canTouchEfiVariables = true;
      grub = {
        enable = true;
        default = "0";
        useOSProber = true;
        efiSupport = true;
        device = "nodev";
        configurationLimit = 20;
      };
    };
    supportedFilesystems = [ "ntfs" ];
  };

  # FIXME(Johannes): load this "lazily"?  It's not necessary for the boot
  # process, but right now if mounting this fails, the PC won't start.
  # fileSystems."/music" = {
  #   device = "/dev/sda1";
  #   fsType = "ntfs";
  #   options = [ "rw" "uid=1000" ];
  # };

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
    udev.packages = [ pkgs.qmk-udev-rules ];
    xserver = {
      enable = true;
      videoDrivers = [ "nvidia" ];
      # The default value that is detected is too high; alternative
      # would be to set this in i3 startup, but this setting here is
      # just right for this machine.
      dpi = 96;
      libinput = {
        enable = true;
        mouse.naturalScrolling = true;
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
        autoLogin = {
          enable = true;
          user = username;
        };
        lightdm = {
          enable = true;
          greeters.mini.enable = true;
          greeters.mini.user = username;
        };
      };
    };
    pipewire = {
      enable = true;
      pulse.enable = true;
      alsa.enable = false;
    };
  };

  programs = {
    ssh.startAgent = true;
    fish.enable = true;
  };

  documentation = {
    enable = true;
    man.enable = true;
    dev.enable = true;
  };

  hardware = {
    opengl = {
      enable = true;
      driSupport32Bit = true;
    };
  };

  users.users.${username} = {
    uid = 1000;
    isNormalUser = true;
    home = "/home/${username}";
    extraGroups = [ "wheel" "networkmanager" "docker" ];
    shell = pkgs.fish;
  };

  environment = {
    systemPackages = with pkgs; [ ];
    pathsToLink = [ "/share/zsh" ];
  };

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
