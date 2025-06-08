{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) mkEnableOption;

  cfg = config.homelab.shiori;
in

{
  options.homelab.shiori = {
    enable = mkEnableOption "Shiori";
  };

  config = lib.mkIf cfg.enable {
    services.shiori = {
      enable = true;
    };
    environment.systemPackages = with pkgs; [
      shiori
    ];
  };

}
