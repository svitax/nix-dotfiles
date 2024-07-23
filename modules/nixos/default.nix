rec {
  storage = {
    onedrive = ./storage/onedrive;
  };

  style = {
    stylix = ./style/stylix;
  };

  window-manager = {
    gnome = ./window-manager/gnome;
    lightdm = ./window-manager/lightdm;
    wayland = ./window-manager/wayland;
  };

  allModules = {
    imports = [
      storage.onedrive

      style.stylix

      window-manager.gnome
      window-manager.lightdm
      window-manager.wayland
    ];
  };
}
