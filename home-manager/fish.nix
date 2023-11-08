{ pkgs, ... }:
{
  programs.fish = {
    enable = true;
    shellAbbrs = {
      nixrs = "sudo nixos-rebuild --flake . switch";
      nv = "nvim";
      vim = "nvim";
      vi = "nvim";
      v = "nvim";
    };
    shellAliases = {
      # Clear screen and scrollback
      clear = "printf '\\033[2J\\033[3J\\033[1;1H'";
    };
    functions = {
      # Disable greeting
      fish_greeting = "";
    };
    interactiveShellInit = /* fish */ ''
      	# Use vim bindings and cursors
      	fish_vi_key_bindings
      	set fish_cursor_default block blink
      	set fish_cursor_insert line blink
      	set fish_cursor_replace_one underscore blink
      	set fish_cursor_visual block
      	'';
    plugins = [
      # Enable a plugin from nixpkgs
      { name = "autopair"; src = pkgs.fishPlugins.autopair.src; }
      { name = "sponge"; src = pkgs.fishPlugins.sponge.src; }
      { name = "puffer"; src = pkgs.fishPlugins.puffer.src; }
      { name = "colored_man_pages"; src = pkgs.fishPlugins.colored-man-pages.src; }
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
