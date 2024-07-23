{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: let
  inherit (lib) mkEnableOption mkOption types;

  cfg = config.storage.onedrive;
in {
  options.storage.onedrive = {
    enable = mkEnableOption "OneDrive";
  };

  config = lib.mkIf cfg.enable {
    services.onedrive.enable = true;
  };
}
