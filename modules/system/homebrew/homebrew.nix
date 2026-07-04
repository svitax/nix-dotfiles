{
  ...
}:
{
  flake.modules.darwin.homebrew = {
    homebrew = {
      enable = true;
      casks = [
        "ghostty"
      ];
    };
  };
}
