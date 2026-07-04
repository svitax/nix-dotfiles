{
  inputs,
  ...
}:
let
  username = "evermind";
in
{
  flake.modules.nixos.${username} =
    { pkgs, ... }:
    {
      users.users.${username} = {
        isNormalUser = true;
        description = username;
        extraGroups = [ "networkmanager" "wheel" ];
      };

      home-manager.users.${username} = {
        imports = [ inputs.self.modules.homeManager.${username} ];
      };
    };
}
