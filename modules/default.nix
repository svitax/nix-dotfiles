{self, ...}: let
  flakeModules = "${self}/modules";
in {
  flake = {
    nixosModules = {};

    # homeManagerModules = rec {
    # 			 wm = {
    # 			    gnome = ./home-manager/wm/gnome;
    # 			 };
    # 			 allModules = {
    # 			 	    imports = [
    # 				    	    wm.gnome
    # 				    ];
    # 			 };
    # };
    # homeManagerModules = {
    # 			 gnome = "${flakeModules}/home-manager/gnome";
    # };
  };
}
