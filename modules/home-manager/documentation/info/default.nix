{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) mkEnableOption;

  cfg = config.modules.documentation.info;
in
{
  options.modules.documentation.info = {
    enable = mkEnableOption "";
  };

  config = lib.mkIf cfg.enable {
    programs.info.enable = true;

    home.packages = with pkgs; [
      info-files
    ];
  };
}
