{
  config,
  lib,
  ...
}:
let
  inherit (lib) mkEnableOption;

  cfg = config.desktop.firefox;
in
{
  options.desktop.firefox = {
    enable = mkEnableOption "Firefox web browser";
  };

  config = lib.mkIf cfg.enable {
    programs.firefox = {
      enable = true;
    };
  };
}
