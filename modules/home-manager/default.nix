rec {
  commands = {
    direnv = ./commands/direnv;
  };

  desktop = {
    firefox = ./desktop/firefox;
    foot = ./desktop/foot;
    rofi = ./desktop/rofi;
  };

  editor = {
    emacs = ./editor/emacs;
  };

  shell = {
    bash = ./shell/bash;
  };

  style = {
    fonts = ./style/fonts;
    stylix = ./style/stylix;
  };

  window-manager = {
    sway = ./window-manager/sway;
  };

  allModules = {
    imports = [
      commands.direnv

      desktop.firefox
      desktop.foot
      desktop.rofi

      editor.emacs

      shell.bash

      style.fonts
      style.stylix

      window-manager.sway
    ];
  };
}
