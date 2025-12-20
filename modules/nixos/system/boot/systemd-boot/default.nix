{
  lib,
  config,
  ...
}:
let
  cfg = config.modules.system.boot.systemd-boot;
in
{
  options.modules.system.boot.systemd-boot = {
    enable = lib.mkEnableOption "Enable systemd-boot bootloader.";
  };

  config = lib.mkIf cfg.enable {
    # Bootloader.
    boot.loader.systemd-boot.enable = true;
    boot.loader.efi.canTouchEfiVariables = true;
  };
}
