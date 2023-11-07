{ pkgs, ... }:
{
  programs.fish = {
    enable = true;
    shellAbbrs = {
      nixrs = "nixos-rebuild --flake . switch";
      hms = "home-manager --flake . switch";
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
    ];
  };
}
