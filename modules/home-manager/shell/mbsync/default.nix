{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) mkEnableOption;

  cfg = config.modules.shell.mbsync;
in
{
  options.modules.shell.mbsync = {
    enable = mkEnableOption "";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      isync
    ];

    home.file.".mbsyncrc".source = ./.mbsyncrc;

    systemd.user.services.mbsync = {
      Unit = {
        Description = "Mailbox synchronization service";
      };
      Service = {
        Type = "oneshot";
        ExecStart = "${pkgs.isync}/bin/mbsync -a";
        ExecStartPost = "${pkgs.notmuch}/bin/notmuch new";
      };
    };

    systemd.user.timers.mbsync = {
      Unit = {
        Description = "Mailbox synchronization timer";
      };
      Timer = {
        OnBootSec = "2m";
        OnUnitActiveSec = "2m";
        Unit = "mbsync.service";
      };
      Install = {
        WantedBy = [ "timers.target" ];
      };
    };
  };
}
