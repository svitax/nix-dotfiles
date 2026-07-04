{
  ...
}:
{
  flake.modules.homeManager.timewarrior =
    { pkgs, ... }:
    {
      home.packages = [ pkgs.timewarrior ];
    };
}
