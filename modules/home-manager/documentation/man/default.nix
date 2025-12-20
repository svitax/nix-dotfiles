{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) mkEnableOption;

  cfg = config.modules.documentation.man;
in
{
  options.modules.documentation.man = {
    enable = mkEnableOption "";
  };

  config = lib.mkIf cfg.enable {
    programs.man = {
      enable = true;
      generateCaches = true;
    };
    home.packages = with pkgs; [
      man-pages
    ];
  };
}
