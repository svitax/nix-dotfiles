{
  inputs,
  ...
}:
{
  # Uncomment when tachyon (NixOS server/NAS) is ready:
  # flake.modules.nixos.tachyon = {
  #   imports = with inputs.self.modules.nixos; [
  #     nix-settings
  #     evermind
  #   ];
  #   networking.hostName = "tachyon";
  #   system.stateVersion = "24.11";
  # };
}
