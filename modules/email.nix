{ config, lib, pkgs, ... }:

with lib;
let
  realName = "Johannes Maier";
  cfg = config.modules.email;
in with import <home-manager/modules/lib/dag.nix> { inherit lib; }; {
  options.modules.email = {
    enable = mkEnableOption "email module";

    maildir = mkOption {
      type = types.str;
      default = ".mail";
    };

    certificatesFile = mkOption {
      type = types.nullOr types.str;
      default = null;
    };

    primaryAccount = mkOption {
      type = types.enum [ "ag" "mailbox" ];
      default = "mailbox";
    };

    isSyncServer = mkOption {
      type = types.bool;
      default = false;
    };
  };

  # TODO notmuch configuration
  # TODO afew?

  config = mkIf cfg.enable {
    # TODO enable pass and automatically prompt for mail address entries
    accounts.email = {
      maildirBasePath = cfg.maildir;
      certificatesFile = cfg.certificatesFile;
      accounts = {
        mailbox = rec {
          address = "johannes.maier@mailbox.org";
          userName = address;
          primary = cfg.primaryAccount == "mailbox";
          mbsync = {
            enable = cfg.isSyncServer;
            create = "maildir";
            expunge = "both";
          };
          msmtp.enable = !cfg.isSyncServer;
          notmuch.enable = true;
          inherit realName;
          passwordCommand = "pass show email/johannes.maier@mailbox.org";
          imap = {
            host = "imap.mailbox.org";
            port = 993;
            tls.enable = true;
          };
          smtp = {
            host = "smtp.mailbox.org";
            port = 465;
            tls.enable = true;
          };
        };
        ag = {
          address = "johannes.maier@active-group.de";
          userName = "maier";
          primary = cfg.primaryAccount == "ag";
          mbsync = {
            enable = cfg.isSyncServer;
            create = "maildir";
            expunge = "both";
          };
          msmtp.enable = !cfg.isSyncServer;
          notmuch.enable = true;
          inherit realName;
          passwordCommand = "pass show email/johannes.maier@active-group.de";
          imap = {
            host = "imap.active-group.de";
            port = null;
            tls = {
              enable = true;
              useStartTls = true;
            };
          };
          smtp = {
            host = "smtp.active-group.de";
            port = null;
          };
        };
      };
    };

    home.packages = [ pkgs.muchsync ];

    programs.notmuch = if cfg.isSyncServer then {
      enable = true;
      hooks = {
        preNew = "mbsync --all";
        postNew = ''
          notmuch tag +work -- tag:new and to:johannes.maier@active-group.de
          notmuch tag +private -- tag:new and to:johb.maier@gmail.com
          notmuch tag +private -- tag:new and to:johannes.maier@mailbox.org
          notmuch tag +sent -- tag:new and from:johb.maier@mailbox.org
          notmuch tag +sent -- tag:new and from:johannes.maier@mailbox.org
          notmuch tag +sent -- tag:new and from:johannes.maier@active-group.de
          notmuch tag -new +unread +inbox -- tag:new
        '';
      };
      new.tags = [ "new" ];
    } else {
      # Need this for muchsync to work.
      enable = true;
    };

    services.muchsync = mkIf (!cfg.isSyncServer) {
      remotes.syncRoot = {
        remote.host = "sync";
        frequency = "*:0/5";
      };
    };
  };
}
