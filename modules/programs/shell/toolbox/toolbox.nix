{
  inputs,
  ...
}:
{
  flake.modules.homeManager.toolbox =
    { ... }:
    {
      imports = [ inputs.amzn-community.homeModules.default ];
      programs.toolbox.enable = true;
      programs.toolbox.cr.enable = true;
    };
}
