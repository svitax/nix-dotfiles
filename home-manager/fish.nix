{ pkgs, ... }:
{
  programs.fish = {
    enable = true;
    interactiveShellInit = ''
    	set fish_greeting # Disable greeting
	'';
    plugins = [
    	# Enable a plugin from nixpkgs
	{ name = "autopair"; src = pkgs.fishPlugins.autopair.src; }
    ];
  };
}
