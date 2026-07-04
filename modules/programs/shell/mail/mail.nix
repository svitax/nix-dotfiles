{
  ...
}:
{
  flake.modules.homeManager.mail =
    { pkgs, ... }:
    {
      home.packages = with pkgs; [
        isync
        msmtp
        notmuch
      ];

      home.file.".mbsyncrc".source = ./mbsyncrc;
      home.file.".config/msmtp/config".source = ./msmtp-config;
      home.file.".notmuch-config".source = ./notmuch-config;
      home.file."mail/.notmuch/hooks/pre-new" = {
        source = ./notmuch-pre-new;
        executable = true;
      };
      home.file."mail/.notmuch/hooks/post-new" = {
        source = ./notmuch-post-new;
        executable = true;
      };

      systemd.user.services.mbsync = {
        Unit.Description = "Mailbox synchronization service";
        Service = {
          Type = "oneshot";
          ExecStart = "${pkgs.isync}/bin/mbsync -Va";
          ExecStartPost = "${pkgs.notmuch}/bin/notmuch new";
        };
        Install.WantedBy = [ "default.target" ];
      };

      systemd.user.timers.mbsync = {
        Unit.Description = "Mailbox synchronization timer";
        Timer = {
          OnBootSec = "2m";
          OnUnitActiveSec = "2m";
          Unit = "mbsync.service";
        };
        Install.WantedBy = [ "timers.target" ];
      };
    };
}
