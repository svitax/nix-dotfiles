{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) mkEnableOption;

  cfg = config.modules.services.dropbox;
in
{
  options.modules.services.dropbox = {
    enable = mkEnableOption "";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      nh
      maestral
      # maestral-gui
    ];

    systemd.user.services.maestral = {
      Unit = {
        Description = "Maestral daemon (Dropbox)";
      };
      Install = {
        WantedBy = [ "default.target" ];
      };
      Service = {
        ExecStart = "${pkgs.maestral}/bin/maestral start --foreground";
        ExecStop = "${pkgs.maestral}/bin/maestral stop";
        Restart = "on-failure";
      };
    };
  };
}
