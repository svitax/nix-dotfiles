{pkgs, ...}: {
  programs.fish = {
    enable = true;
    shellAbbrs = {
      ll = "eza --no-user --header --group-directories-first --all --binary";
      lla = "eza --no-user --header --group-directories-first --all --binary --long";
      llg = "eza --no-user --header --group-directories-first --all --binary --long --git-ignore";
      llt = "eza --no-user --header --group-directories-first --tree --level 2";
      # sw = "sudo nixos-rebuild --flake . switch";
      sw = "nh os switch --nom .";
      swd = "nh os switch --nom --dry .";
      hm = "nh home switch --nom .";
      hmd = "nh home switch --nom --dry .";
      cla = "nh clean all --keep 3";
      clad = "nh clean all --keep 3 --dry";
      clu = "nh clean user --keep 3";
      clud = "nh clean user --keep 3 --dry";
      use = "nix-shell --command fish -p";
      nv = "nvim";
      vim = "nvim";
      vi = "nvim";
      v = "nvim";
      neogit = "nvim -c :Neogit";
      ngit = "nvim -c :Neogit";
      ng = "nvim -c :Neogit";
      diffview = "nvim -c :DiffviewOpen";
      ndiff = "nvim -c :DiffviewOpen";
      nlog = "nvim -c :DiffviewFileHistory";
      lg = "lazygit";
      si = "sioyek";
      zj = "zellij";
      zs = "zellij-smart-sessionizer";
      cl = "clear";
      dot = "cd ~/nix-dotfiles";
      prj = "cd ~/projects";
    };
    shellAliases = {
      # Clear screen and scrollback
      clear = "printf '\\033[2J\\033[3J\\033[1;1H'";
    };
    functions = {
      # Disable greeting
      fish_greeting = "";
      flakify = {
        body = ''
          if not test -e flake.nix
            nix flake new -t github:nix-community/nix-direnv .
          else if not test -e .envrc
            echo "use flake" > .envrc
            direnv allow
          end
          $EDITOR flake.nix
        '';
      };
      subcommand_abbr = {
        body =
          # fish
          ''
            # `g co`, `npm i`, etc. subcommand expansion with `abbr`.
            set -l cmd "$argv[1]"
            set -l short "$argv[2]"
            set -l long "$argv[3]"

            # Check that these strings are safe, since we're going to eval. 👺
            if not string match --regex --quiet '^[a-z]*$' "$short"
              or not string match --regex --quiet '^[a-z- ]*$' "$long"
              echo "Scary unsupported alias or expansion $short $long"; exit 1;
            end

            set -l abbr_temp_fn_name (string join "_" "abbr" "$cmd" "$short")
            # Subcommand arg expanesion via commandline -tokenize + abbr --position anywhere
            # thx lgarron for inspiration: https://github.com/lgarron/dotfiles/blob/2bc3e0282b/dotfiles/fish/.config/fish/abbr.fish & https://github.com/lgarron/dotfiles/blob/main/dotfiles/fish/.config/fish/dev.fish
            # https://www.reddit.com/r/fishshell/comments/16s0bsi/leveraging_abbr_for_git_aliases/
            set -l abbr_temp_fn "function $abbr_temp_fn_name
              set --local tokens (commandline --tokenize)
              if test \$tokens[1] = \"$cmd\"
                echo $long
              else
                echo $short
              end;
            end;
            abbr --add $short --position anywhere --function $abbr_temp_fn_name"
            eval "$abbr_temp_fn"
          '';
      };
    };
    interactiveShellInit =
      # fish
      ''
        # Set default editor because home.sessionVariables and environment.variables doesn't get picked up by fish
        # due to this https://nix-community.github.io/home-manager/index.html#why_are_the_session_variables_not_set
        # (home-manager isn't managing my fish shell configuration)
        set -gx EDITOR "nvim"
        set -gx VISUAL "nvim"

        # Use vim bindings and cursors
        fish_vi_key_bindings
        set fish_cursor_default block blink
        set fish_cursor_insert line blink
        set fish_cursor_replace_one underscore blink
        set fish_cursor_visual block

        subcommand_abbr git c "commit -am"
        subcommand_abbr npm i "install"

        bind \ef -M insert 'commandline "zellij-smart-sessionizer" && commandline -f execute && commandline -f repaint'
      '';
    plugins = [
      # Enable a plugin from nixpkgs
      {
        name = "autopair";
        src = pkgs.fishPlugins.autopair.src;
      }
      {
        name = "sponge";
        src = pkgs.fishPlugins.sponge.src;
      }
      {
        name = "puffer";
        src = pkgs.fishPlugins.puffer.src;
      }
      {
        name = "colored_man_pages";
        src = pkgs.fishPlugins.colored-man-pages.src;
      }
      # nurl (for rev and hash)
      {
        name = "fish-gqh";
        src = pkgs.fetchFromGitHub {
          owner = "decors";
          repo = "fish-ghq";
          rev = "cafaaabe63c124bf0714f89ec715cfe9ece87fa2";
          hash = "sha256-6b1zmjtemNLNPx4qsXtm27AbtjwIZWkzJAo21/aVZzM=";
        };
      }
      # {
      #   name = "fifc";
      #   src = pkgs.fetchFromGitHub {
      #     owner = "gazorby";
      #     repo = "fifc";
      #     rev = "95fc121e245a783ec0b3a235d320a84f1f87ccfd";
      #     hash = "sha256-p5E4Mx6j8hcM1bDbeftikyhfHxQ+qPDanuM1wNqGm6E=";
      #   };
      # }
      # {
      #   name = "colored_man_pages";
      #   src = pkgs.fetchFromGitHub {
      #     owner = "PatrickF1";
      #     repo = "colored_man_pages.fish";
      #     rev = "f885c2507128b70d6c41b043070a8f399988bc7a";
      #     sha256 = "ii9gdBPlC1/P1N9xJzqomrkyDqIdTg+iCg0mwNVq2EU=";
      #   };
      # }
    ];
  };
  # TODO: figure out if i ever need to refresh this in the future
  # i generated poetry.fish with `poetry completions fish > ./poetry.fish`
  # and then removed mangled double quotes with `sed -i.bak -E "s/'([a-z]*[[:blank:]][a-z]*)''/\1'/g" ./poetry.fish
  # https://github.com/python-poetry/poetry/issues/5929
  home.file.".config/fish/completions/poetry.fish".source = ./config/fish/completions/poetry.fish;
}
