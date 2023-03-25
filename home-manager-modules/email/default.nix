{ config, lib, pkgs, ... }:

let
  types = lib.types;
  realName = "Johannes Maier";
  cfg = config.modules.email;
in {
  options.modules.email = {
    enable = lib.mkEnableOption "email module";

    maildir = lib.mkOption {
      type = types.str;
      default = ".mail";
    };

    certificatesFile = lib.mkOption {
      type = types.nullOr types.str;
      default = null;
    };

    primaryAccount = lib.mkOption {
      type = types.enum [ "ag" "mailbox" ];
      default = "mailbox";
    };
  };

  config = lib.mkIf cfg.enable {
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
            enable = true;
            create = "both";
            remove = "both";
            expunge = "both";
            patterns = [ "*" "!Drafts" ];
          };
          msmtp = {
            enable = true;
            extraConfig = { "syslog" = "LOG_USER"; };
          };
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
            enable = true;
            create = "both";
            remove = "both";
            expunge = "both";
            patterns = [ "*" "!Drafts" "!Deleted Messages" ];
          };
          msmtp = {
            enable = true;
            extraConfig = { "syslog" = "LOG_USER"; };
          };
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
    programs = {
      mbsync.enable = true;
      msmtp.enable = true;
      notmuch = {
        enable = true;
        maildir.synchronizeFlags = true;
        search.excludeTags = [ "deleted" "spam" ];
        hooks = {
          postNew = ''
            notmuch tag --batch --input=${./notmuch-initial-tags}
            if [ $(hostname) != "paln" ]; then
                muchsync -vv --nonew sync
            fi'';
          preNew = ''
            if [ $(hostname) != "paln" ]; then
                muchsync -vv --nonew sync
            else
                mbsync --all
            fi'';
        };
      };
    };
  };
}
