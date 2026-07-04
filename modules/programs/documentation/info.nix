{
  ...
}:
{
  flake.modules.homeManager.info =
    { pkgs, ... }:
    {
      programs.info.enable = true;
      home.packages = [ pkgs.python-info ];
    };
}
