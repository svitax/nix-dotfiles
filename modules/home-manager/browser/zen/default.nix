{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
let
  inherit (lib) mkEnableOption;

  cfg = config.browser.zen;
in
{
  options.browser.zen = {
    enable = mkEnableOption "";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      inputs.zen-browser.packages."${system}".default # beta
    ];
  };
}
