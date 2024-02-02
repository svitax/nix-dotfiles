{ pkgs, ... }: {
  imports = [ ../hardware/opengl.nix ];
  environment.systemPackages = [ pkgs.wayland ];
  services.xserver = {
    enable = true;
    layout = "us";
    displayManager.gdm = {
      enable = true;
      wayland = true;
    };
  };
}
