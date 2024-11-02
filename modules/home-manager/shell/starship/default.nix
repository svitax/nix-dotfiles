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
        enableFishIntegration = true;
        enableZshIntegration = true;
        settings = {
          character = {
            success_symbol = "[>](bold green)";
            error_symbol = "[>](bold red)";
          };
          add_newline = false;
          format = promptFormat;
          right_format = rightPromptFormat;
          shell = {
            disabled = true;
            format = "$indicator";
            fish_indicator = "";
            bash_indicator = "[BASH](bright-white) ";
            zsh_indicator = "[ZSH](bright-white) ";
          };
          directory = {
            style = "blue";
            read_only = "  ";
            truncation_length = 4;
            truncate_to_repo = false;
          };
          fill = {
            symbol = " ";
          };
          cmd_duration = {
            format = "[$duration]($style) ";
            style = "yellow";
          };
          jobs = {
            symbol = " ";
            style = "bold red";
            number_threshold = 1;
            format = "[$symbol]($style)";
          };
          aws = {
            symbol = " ";
            format = "[$symbol($profile )(($region) )]($style) ";
            style = "white";
            region_aliases = {
              us-east-1 = "va";
            };
          };
          azure = {
            symbol = " ";
            format = "[$symbol($subscription )]($style) ";
            style = "white";
          };
          gcloud = {
            symbol = " ";
            format = "[$symbol($account )(($region) )]($style) ";
            style = "white";
          };
          docker_context = {
            symbol = " ";
            format = "[$symbol($context )]($style)";
            style = "white";
          };
          golang = {
            symbol = " ";
            format = "[$symbol($version )]($style)";
            style = "white";
          };
          lua = {
            symbol = " ";
            format = "[$symbol($version )]($style)";
            style = "white";
          };
          nix_shell = {
            symbol = " ";
            format = "[$symbol($name )]($style)";
            # style = "fg:#${colors.base03}";
            style = "fg:${config.lib.stylix.colors.withHashtag.base03}";
          };
          nodejs = {
            symbol = " ";
            format = "[$symbol($version )]($style)";
            style = "white";
          };
          python = {
            symbol = " ";
            # format = "[$symbol($virtualenv )]($style) ";
            format = "[$symbol($version )]($style) ";
            style = "white";
            python_binary = [
              "./venv/bin/python"
              "python"
              "python3"
              "python2"
            ];
          };
          rust = {
            symbol = " ";
            format = "[$symbol($version )]($style)";
            style = "white";
          };
          shlvl = {
            symbol = "λ ";
            format = "[$symbol($shlvl )]($style)";
            style = "white";
          };
        };
      };
    };
}
