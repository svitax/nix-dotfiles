{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) mkEnableOption;

  cfg = config.modules.shell.mbsync;
in
{
  options.modules.shell.mbsync = {
    enable = mkEnableOption "";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      isync
    ];

    home.file.".mbsyncrc".source = ./.mbsyncrc;
  };
}
