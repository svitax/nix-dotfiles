{
  lib,
  config,
  inputs,
  ...
}:
let
  cfg = config.nix-settings;
in
{
  options.nix-settings = {
    enable = lib.mkEnableOption "Configures Nix options.";
  };

  config = lib.mkIf cfg.enable {
    # Apply overlays
    nixpkgs.overlays = [
      inputs.self.overlays.packages
    ];
  };
}
