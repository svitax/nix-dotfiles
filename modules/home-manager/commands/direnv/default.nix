{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: let
  inherit (lib) mkEnableOption mkOption types;

  cfg = config.commands.direnv;
in {
  options.commands.direnv = {
    enable = mkEnableOption "Direnv";
  };

  config = lib.mkIf cfg.enable {
    programs.direnv = {
      enable = true;
      nix-direnv = {enable = true;};
    };
  };
}
