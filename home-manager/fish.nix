{ pkgs, ... }:
{
  programs.fish = {
    enable = true;
    interactiveShellInit = ''
    	set fish_greeting # Disable greeting
    shellAbbrs = {
    	nixrs = "nixos-rebuild --flake . switch";
	hms = "home-manager --flake . switch";
	vim = "nvim";
	vi = "nvim";
	v = "nvim";
    };
	'';
    plugins = [
    	# Enable a plugin from nixpkgs
	{ name = "autopair"; src = pkgs.fishPlugins.autopair.src; }
    ];
  };
}
