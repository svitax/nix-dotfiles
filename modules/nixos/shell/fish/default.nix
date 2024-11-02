{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) mkEnableOption;

  cfg = config.shell.fish;
in
{
  options.shell.fish = {
    enable = mkEnableOption "Fish";
  };

  config = lib.mkIf cfg.enable {
    programs.fish.enable = true;
    users.defaultUserShell = pkgs.fish;
  };
}
