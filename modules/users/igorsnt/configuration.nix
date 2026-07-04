{
  inputs,
  ...
}:
let
  username = "igorsnt";
in
{
  flake.modules.darwin.${username} =
    { ... }:
    {
      users.users.${username}.home = "/Users/${username}";
      home-manager.users.${username} = {
        imports = [
          inputs.self.modules.homeManager.${username}
        ];
      };
    };
}
