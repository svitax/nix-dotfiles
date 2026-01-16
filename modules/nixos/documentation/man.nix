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
    documentation.man = {
      enable = true;
      generateCaches = true;
    };
    environment.systemPackages = with pkgs; [
      man-pages
      man-pages-posix
    ];
  };
}
