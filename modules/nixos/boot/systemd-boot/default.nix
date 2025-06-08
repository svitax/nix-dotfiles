{
  lib,
  config,
  inputs,
  ...
}:
let
  cfg = config.boot.systemd-boot;
in
{
  options.boot.systemd-boot = {
    enable = lib.mkEnableOption "Enable systemd-boot bootloader.";
  };

  config = lib.mkIf cfg.enable {
    # Bootloader.
    boot.loader.systemd-boot.enable = true;
    boot.loader.efi.canTouchEfiVariables = true;
  };
}