{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: let
  inherit (lib) mkEnableOption mkOption types;

  cfg = config.desktop.foot;
in {
  options.desktop.foot = {
    enable = mkEnableOption "Foot terminal emulator";
  };

  config = lib.mkIf cfg.enable {
    programs.foot = {
      enable = true;
      server.enable = true;
      settings = {
        main = {
          font = "Iosevka Comfy:size=18";
        };
      };
    };
  };
}
