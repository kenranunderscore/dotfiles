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
  nixpkgs.config.allowUnfree = true;

  imports = [ ./hardware-configuration.nix ];

  boot = {
    kernelPackages = pkgs.linuxPackages_latest;
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;
  };

  networking = {
    hostName = "angband";
    networkmanager.enable = true;
  };

  time.timeZone = "Europe/Berlin";

  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };
  i18n.defaultLocale = "en_US.UTF-8";

  services = {
    xserver = {
      enable = true;
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

  programs.ssh.startAgent = true;

  documentation = {
    enable = true;
    man.enable = true;
    dev.enable = true;
  };

  hardware = { opengl.enable = true; };

  users.users.${username} = {
    isNormalUser = true;
    home = "/home/${username}";
    extraGroups = [ "wheel" "networkmanager" ];
    shell = pkgs.fish;
  };

  environment = {
    systemPackages = with pkgs; [ ];
    pathsToLink = [ "/share/zsh" ];
  };

  networking.firewall.enable = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.05"; # Did you read the comment?
}
