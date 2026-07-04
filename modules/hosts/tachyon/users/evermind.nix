# Host-specific binding for evermind on tachyon (server)
# Headless/server profile — no desktop, just CLI tools.
{
  ...
}:
{
  # flake.modules.nixos.evermind-tachyon = { ... }: {
  #   # tachyon-specific: CLI only, no GUI
  #   home-manager.users.evermind.imports = with inputs.self.modules.homeManager; [
  #     # git
  #     # starship
  #   ];
  # };
}
