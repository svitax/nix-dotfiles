{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) mkEnableOption;

  cfg = config.modules.shell.msmtp;
in
{
  options.modules.shell.msmtp = {
    enable = mkEnableOption "";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      msmtp
    ];

    home.file.".config/msmtp/config".source = ./config;
  };
}
