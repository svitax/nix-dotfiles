{
  config,
  lib,
  ...
}:
let
  inherit (lib) mkEnableOption;

  cfg = config.modules.services.onedrive;
in
{
  options.modules.services.onedrive = {
    enable = mkEnableOption "OneDrive";
  };

  config = lib.mkIf cfg.enable {
    services.onedrive.enable = true;
  };
}
