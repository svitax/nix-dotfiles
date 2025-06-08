{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) mkEnableOption;

  cfg = config.command-line.man;
in
{
  options.command-line.man = {
    enable = mkEnableOption "";
  };

  config = lib.mkIf cfg.enable {
    programs.man = {
      enable = true;
      generateCaches = true;
    };
  };
}
