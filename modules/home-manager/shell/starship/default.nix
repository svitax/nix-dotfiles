{
  config,
  lib,
  ...
}:
let
  inherit (lib) mkEnableOption;

  cfg = config.shell.starship;
in
{
  options.shell.starship = {
    enable = mkEnableOption "Starship prompt";
  };

  config =
    let
      promptOrder = [
        # "shlvl"
        # "shell"
        "nix_shell"
        "username"
        "hostname"
        "directory"
        "character"
      ];
      # BUG this breaks meow-vterm integration
      rightPromptOrder = [
        # "aws"
        # "docker_context"
        # "python"
        # "nodejs"
        # "lua"
        # "golang"
        # "rust"
        # "cmd_duration "
        # "jobs"
      ];
      promptFormat = lib.concatStrings (map (s: "\$${s}") promptOrder);
      rightPromptFormat = lib.concatStrings (map (s: "\$${s}") rightPromptOrder);
    in
    lib.mkIf cfg.enable {
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
          right_format = rightPromptFormat;
          nix_shell = {
            symbol = "❄";
            format = "[$symbol($name )]($style)";
          };
        };
      };
    };
}
