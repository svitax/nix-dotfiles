{
  lib,
  config,
  inputs,
  ...
}:
let
  cfg = config.modules.nix.nixpkgs;
in
{
  options.modules.nix.nixpkgs = {
    enable = lib.mkEnableOption "Configures Nix options.";
  };

  config = lib.mkIf cfg.enable {
    # Apply overlays
    nixpkgs.overlays = [
      inputs.self.overlays.packages
    ];
  };
}
