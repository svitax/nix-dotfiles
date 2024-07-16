{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: let
  inherit (lib) mkEnableOption mkOption types;

  cfg = config.desktop.rofi;
in {
  options.desktop.rofi = {
    enable = mkEnableOption "Rofi";
  };

  config = lib.mkIf cfg.enable {
    programs.rofi = {
      enable = true;
      package = pkgs.rofi-wayland;
      font = "Iosevka Comfy 18";
      terminal = config.window-manager.sway.terminal;
    };
  };
}
