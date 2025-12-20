{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) mkEnableOption;

  cfg = config.modules.nix;
in
{
  options.modules.nix = {
    enable = mkEnableOption "Configures Nix options.";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      nh
    ];

    # Apply overlays
    nixpkgs.overlays = [
      inputs.self.overlays.packages
    ];
  };
}
