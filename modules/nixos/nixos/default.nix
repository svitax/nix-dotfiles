{
  lib,
  config,
  inputs,
  ...
}:
let
  cfg = config.nixos;
in
{
  options.nixos = {
    enable = lib.mkEnableOption "Configures Nix options.";
  };

  config = lib.mkIf cfg.enable {

      nix.nixPath = [ "nixpkgs=${inputs.nixpkgs}" ]; # need for nixd
      nix.settings.experimental-features = "nix-command flakes";

      # Allow unfree packages
      nixpkgs.config.allowUnfree = true;
  };
}
