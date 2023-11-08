{pkgs, ...}: {
  programs.fish = {
    enable = true;
    shellAbbrs = {
      lla = "eza --no-user --header --group-directories-first --icons --all --binary --long --git";
      llg = "eza --no-user --header --group-directories-first --icons --all --binary --long --git --git-ignore";
      llt = "eza --no-user --header --group-directories-first --icons --tree --level 2";
      nixrs = "sudo nixos-rebuild --flake . switch";
      nv = "nvim";
      vim = "nvim";
      vi = "nvim";
      v = "nvim";
      cl = "clear";
    };
    shellAliases = {
      # Clear screen and scrollback
      clear = "printf '\\033[2J\\033[3J\\033[1;1H'";
    };
    functions = {
      # Disable greeting
      fish_greeting = "";
    };
    interactiveShellInit =
      /*
      fish
      */
      ''
               # Use vim bindings and cursors
               fish_vi_key_bindings
               set fish_cursor_default block blink
               set fish_cursor_insert line blink
               set fish_cursor_replace_one underscore blink
               set fish_cursor_visual block

        # `g co`, `npm i`, etc. subcommand expansion with `abbr`.
        function subcommand_abbr
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
        end

        subcommand_abbr git c "commit -am"
        subcommand_abbr npm i "install"
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
      # nix-prefetch-github (for rev and hash)
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
}
