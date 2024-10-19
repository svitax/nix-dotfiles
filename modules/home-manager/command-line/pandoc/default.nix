{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: let
  inherit (lib) mkEnableOption mkOption types;

  cfg = config.command-line.pandoc;
in {
  options.command-line.pandoc = {
    enable = mkEnableOption "";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      pandoc
    ];
  };
}
