{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) mkEnableOption;

  cfg = config.modules.services.shiori;
in

{
  options.modules.services.shiori = {
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
