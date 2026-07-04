{
  ...
}:
{
  flake.modules.homeManager.direnv =
    { ... }:
    {
      programs.direnv = {
        enable = true;
        enableBashIntegration = true;
        nix-direnv.enable = true;
      };
      xdg.configFile."direnv/direnv.toml".source = ./direnv.toml;
      home.sessionVariables.DIRENV_LOG_FORMAT = "";
    };
}
