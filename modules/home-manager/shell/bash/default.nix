{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: let
  inherit (lib) mkEnableOption mkOption types;

  cfg = config.shell.bash;
in {
  options.shell.bash = {
    enable = mkEnableOption "Bash";
  };

  config = lib.mkIf cfg.enable {
    programs.bash = {
      enable = true;
    };
  };
}
