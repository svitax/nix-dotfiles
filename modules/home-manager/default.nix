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
    neovim = ./editor/neovim;
  };

  shell = {
    bash = ./shell/bash;
    fzf = ./shell/fzf;
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
      editor.neovim

      shell.bash
      shell.fzf

      style.fonts
      style.stylix

      window-manager.sway
    ];
  };
}
