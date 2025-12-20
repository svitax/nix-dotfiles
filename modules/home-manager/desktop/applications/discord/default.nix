{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) mkEnableOption;

  cfg = config.modules.desktop.applications.discord;
in
{
  options.modules.desktop.applications.discord = {
    enable = mkEnableOption "";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      # (discord.override {
      #   # withOpenASAR = true;
      #   withVencord = true;
      # })
      discord
    ];
  };
}
