{ config, lib, pkgs, ... }:

with lib;
let
  realName = "Johannes Maier";
  cfg = config.modules.email;
in {
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
            patterns = [ "*" "!Drafts" ];
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
            patterns = [ "*" "!Drafts" "!Deleted Messages" ];
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

    programs.notmuch = {
      enable = true;
      hooks = if cfg.isSyncServer then {
        preNew = "mbsync --all";
        postNew = "notmuch tag --batch --input=${./notmuch-initial-tags}";
      } else {
        preNew = "muchsync --nonew sync";
      };
      new.tags = [ "new" ];
    };

    services.muchsync = mkIf (!cfg.isSyncServer) {
      remotes.syncRoot = {
        remote = {
          host = "sync";
          importNew = true;
        };
        local.importNew = false;
        frequency = "*:0/5";
        upload = true;
      };
    };
  };
}
