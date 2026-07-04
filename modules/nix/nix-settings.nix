{
  inputs,
  ...
}:
let
  shared = {
    nix.settings.experimental-features = "nix-command flakes";
    nixpkgs.config.allowUnfree = true;
  };
in
{
  flake.modules.darwin.nix-settings = {
    imports = [ shared ];
  };

  flake.modules.nixos.nix-settings = {
    imports = [ shared ];
    nix.nixPath = [ "nixpkgs=${inputs.nixpkgs}" ];
  };
}
