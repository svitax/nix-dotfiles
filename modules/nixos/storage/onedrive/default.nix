{
  config,
  lib,
  ...
}:
let
  inherit (lib) mkEnableOption;

  cfg = config.storage.onedrive;
in
{
  options.storage.onedrive = {
    enable = mkEnableOption "OneDrive";
  };

  config = lib.mkIf cfg.enable {
    services.onedrive.enable = true;
  };
}
