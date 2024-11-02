{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) mkEnableOption;

  cfg = config.style.stylix;
in
{
  options.style.stylix = {
    enable = mkEnableOption "Stylix";
  };

  config = lib.mkIf cfg.enable {
    stylix.enable = true;
    stylix.autoEnable = false;
    stylix.image = pkgs.fetchurl {
      url = "https://www.pixelstalk.net/wp-content/uploads/2016/05/Epic-Anime;Awesome-Wallpapers.jpg";
      sha256 = "enQo3wqhgf0FEPHj2coOCvo7DuZv+5rL/WIo4qPI50=";
    };
    stylix.base16Scheme = import ../themes/modus-operandi.nix;
  };
}
