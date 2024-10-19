{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: let
  inherit (lib) mkEnableOption mkOption types;

  cfg = config.command-line.nurl;
in {
  options.command-line.nurl = {
    enable = mkEnableOption "";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      nurl
    ];
  };
}
