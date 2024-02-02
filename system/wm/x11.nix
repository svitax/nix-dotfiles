{ pkgs, ... }: {
  imports = [ ./pipewire.nix ];
  services.xserver = {
    enable = true;
    layout = "us";
    xkbVariant = "intl";
    videoDrivers = [ "amdgpu" "radeon" ];
    displayManager.gdm = {
      enable = true;
      # wayland = true;
    };
  };
}
