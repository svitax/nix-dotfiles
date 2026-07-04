{ ... }:
{
  flake.modules.homeManager.git =
    { ... }:
    {
      programs.git = {
        enable = true;
        settings = {
          user.name = "svitax";
          user.email = "svitaxiom@gmail.com";
        };
        ignores = [
          ".direnv/"
          "result"
        ];
      };
    };
}
