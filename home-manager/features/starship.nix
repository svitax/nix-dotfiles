{lib, ...}: let
  promptOrder = ["shlvl" "shell" "nix_shell" "username" "hostname" "directory" "character"];
  rightPromptOrder = ["aws" "docker_context" "python" "nodejs" "lua" "golang" "rust" "cmd_duration " "jobs"];
  promptFormat = lib.concatStrings (map (s: "\$${s}") promptOrder);
  rightPromptFormat = lib.concatStrings (map (s: "\$${s}") rightPromptOrder);
in {
  programs = {
    starship = {
      enable = true;
      enableFishIntegration = true;
      enableBashIntegration = true;
      # enableTransience = true; # can't find option
      # settings = pkgs.lib.importTOML ../starship.toml;
      settings = {
        add_newline = false;
        format = promptFormat;
        right_format = rightPromptFormat;
        # format = ''$shell$nix_shell$username$hostname$directory$character'';
        # right_format = ''$aws$docker_context$python$nodejs$lua$golang$rust$cmd_duration $jobs'';
        shell = {
          disabled = false;
          format = "$indicator";
          fish_indicator = "";
          bash_indicator = "[BASH](bright-white) ";
          zsh_indicator = "[ZSH](bright-white) ";
        };
        directory = {
          style = "blue";
          read_only = " 󰌾";
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
          symbol = "󰸏 ";
          format = "[$symbol($profile )(\($region\) )]($style) ";
          style = "bright-black";
          region_aliases = {
            us-east-1 = "va";
          };
        };
        azure = {
          symbol = " ";
          format = "[$symbol($subscription )]($style) ";
          style = "bright-black";
        };
        gcloud = {
          symbol = "󱇶 ";
          format = "[$symbol($account )(\($region\) )]($style) ";
          style = "bright-black";
        };
        docker_context = {
          symbol = " ";
          format = "[$symbol($context )]($style)";
          style = "bright-black";
        };
        golang = {
          symbol = " ";
          format = "[$symbol($version )]($style)";
          style = "bright-black";
        };
        lua = {
          symbol = " ";
          format = "[$symbol($version )]($style)";
          style = "bright-black";
        };
        nix_shell = {
          symbol = " ";
          format = "[$symbol($name )]($style)";
          style = "bright-black";
        };
        nodejs = {
          symbol = " ";
          format = "[$symbol($version )]($style)";
          style = "bright-black";
        };
        python = {
          symbol = " ";
          # format = "[$symbol($virtualenv )]($style) ";
          format = "[$symbol($version )]($style) ";
          style = "bright-black";
          python_binary = ["./venv/bin/python" "python" "python3" "python2"];
        };
        rust = {
          symbol = " ";
          format = "[$symbol($version )]($style)";
          style = "bright-black";
        };
        shlvl = {
          symbol = "λ ";
          format = "[$symbol($shlvl )]($style)";
          style = "bright-black";
        };
      };
    };
    bash.enable = true;
    # home.file.".config/starship/starship.toml".source = ./starship.toml;
    # using nix-direnv results in a ton of noise when I enter a project directory.
    # set direnv_log_format to an empty string to suppress that noise
    fish.interactiveShellInit = ''
      set -gx DIRENV_LOG_FORMAT ""
    '';
  };
}
