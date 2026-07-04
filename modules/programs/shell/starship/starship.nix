{
  lib,
  ...
}:
{
  flake.modules.homeManager.starship =
    { ... }:
    let
      promptOrder = [
        "nix_shell"
        "username"
        "hostname"
        "directory"
        "character"
      ];
      promptFormat = lib.concatStrings (map (s: "\$\${s}") promptOrder);
    in
    {
      programs.starship = {
        enable = true;
        enableBashIntegration = true;
        settings = {
          character = {
            success_symbol = "[>](bold green)";
            error_symbol = "[>](bold red)";
          };
          add_newline = false;
          format = promptFormat;
          right_format = "";
          nix_shell = {
            symbol = "\u2744";
            format = "[$symbol($name )]($style)";
          };
        };
      };
    };
}
