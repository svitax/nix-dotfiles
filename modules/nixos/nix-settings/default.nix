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
    nix.nixPath = [ "nixpkgs=${inputs.nixpkgs}" ]; # need for nixd
    nix.settings.experimental-features = "nix-command flakes";

    # Allow unfree packages
    nixpkgs.config.allowUnfree = true;

    # Apply overlays
    nixpkgs.overlays = [
      inputs.self.overlays.packages
    ];
  };
}
