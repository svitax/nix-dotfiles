{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) mkEnableOption;

  cfg = config.graphical.discord;
in
{
  options.graphical.discord = {
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
