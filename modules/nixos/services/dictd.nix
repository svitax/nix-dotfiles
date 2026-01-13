{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) mkEnableOption mkOption;

  cfg = config.modules.services.dictd;
in
{
  options.modules.services.dictd = {
    enable = mkEnableOption "Dictionary server";
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ dict ];
    environment.etc."dict.conf".text = "server localhost";

    services.dictd = {
      enable = true;
      DBs = with pkgs; [ dict-gcide ];
    };
  };
}
