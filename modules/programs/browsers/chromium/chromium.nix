{
  ...
}:
{
  flake.modules.homeManager.chromium =
    { pkgs, ... }:
    {
      home.packages = [ pkgs.ungoogled-chromium ];
    };
}
