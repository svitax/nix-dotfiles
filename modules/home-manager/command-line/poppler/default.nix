{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: let
  inherit (lib) mkEnableOption mkOption types;

  cfg = config.command-line.poppler;
in {
  options.command-line.poppler = {
    enable = mkEnableOption "";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      poppler_utils
    ];
  };
}
