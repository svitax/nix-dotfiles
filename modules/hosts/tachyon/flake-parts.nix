{
  inputs,
  ...
}:
{
  # Uncomment when tachyon (NixOS server/NAS) is ready:
  # flake.nixosConfigurations = inputs.self.lib.mkNixos "x86_64-linux" "tachyon";
}
