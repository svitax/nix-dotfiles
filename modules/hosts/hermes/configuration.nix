{
  inputs,
  ...
}:
{
  flake.modules.darwin.hermes = {
    imports = with inputs.self.modules.darwin; [
      nix-settings
      homebrew
      home-manager
      emacs-darwin
      fonts
      igorsnt
    ];
    networking.hostName = "7cf34dea5b4d";
    system.primaryUser = "igorsnt";
    system.stateVersion = 6;
  };
}
