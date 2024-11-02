rec {
  browser = {
    # NOTE: firefox
    # NOTE: nyxt
  };

  clipboard = {
    # NOTE: wl-clipboard (copy/paste utils) https://github.com/bugaevc/wl-clipboard
  };

  command-line = {
    atuin = ./command-line/atuin;
    direnv = ./command-line/direnv;
    fd = ./command-line/fd;
    fzf = ./command-line/fzf;
    git = ./command-line/git;
    nh = ./command-line/nh;
    nurl = ./command-line/nurl;
    pandoc = ./command-line/pandoc;
    poppler = ./command-line/poppler;
    ripgrep = ./command-line/ripgrep;
  };

  compositor = {
    # NOTE: picom (for animations)
  };

  # NOTE: desktop-environment
  desktop-environment = {
    # NOTE: xfce
    # NOTE: shinglify (for autotiling) https://gitlab.com/corthbandt/shinglify
    # NOTE: make retro/copland-esque themes
  };

  # NOTE: rename desktop to graphical
  desktop = {
    firefox = ./desktop/firefox;
    foot = ./desktop/foot;
    # NOTE: hyprpicker (for color picker) https://github.com/hyprwm/hyprpicker
    rofi = ./desktop/rofi;
    # NOTE: swayimg (for image viewer) https://github.com/artemsen/swayimg
  };

  # NOTE: rename editor to text-editor
  editor = {
    emacs = ./editor/emacs;
    neovim = ./editor/neovim;
  };

  hardware = {
    # backlight
    # NOTE: brightnessctl (brightness control) https://github.com/Hummer12007/brightnessctl
    # NOTE: wluma (automatic brightness adjustment) https://github.com/maximbaz/wluma
    # NOTE: lightctl script from avizo for inspiration https://github.com/heyjuvi/avizo/blob/master/lightctl

    # audio
    # NOTE: volumectl script from avizo for inspiration https://github.com/heyjuvi/avizo/blob/master/volumectl

    # media
    # NOTE: playerctl

    # bluetooth
  };

  keyboard = {
    # NOTE: kanata (qmk but for any keyboard) https://github.com/jtroo/kanata or kmonad https://github.com/kmonad/kmonad or xremap https://github.com/xremap/xremap
    # NOTE: swhkd (for hotkeys) https://github.com/waycrate/swhkd or sxhkd https://github.com/baskerville/sxhkd
    # NOTE: keydogger (for text expansion) https://github.com/jarusll/keydogger or espanso https://github.com/espanso/espanso
    # NOTE: wk (for keychord popups) https://github.com/3L0C/wk
  };

  lockscreen = {
    # NOTE: gtklock (for lockscreen) https://github.com/jovanlanik/gtklock or swaylock-effects https://github.com/mortie/swaylock-effects
  };

  shell = {
    bash = ./shell/bash;
    fish = ./shell/fish;
    starship = ./shell/starship;
    zsh = ./shell/zsh;
  };

  style = {
    fonts = ./style/fonts;
    stylix = ./style/stylix;
  };

  wallpaper = {
    # NOTE: swww (animated wallpapers) https://github.com/LGFae/swww
  };

  widgets = {
    # NOTE: ags (for user dashboard and system resource monitor and other widgets)
    # NOTE: ags (for osd) or wob https://github.com/francma/wob
    # NOTE: ags (for notifications) or
    # NOTE: ags (for bar) or waybar

    # NOTE: panel and widgets in ags for inspiration https://github.com/Jas-SinghFSU/HyprPanel
    # NOTE: some widgets in eww for inspiration https://github.com/dharmx/vile
  };

  window-manager = {
    sway = ./window-manager/sway;
    # NOTE: guile-swayer (guile bindings for sway) https://github.com/ebeem/guile-swayer
    # NOTE: mahogany https://github.com/stumpwm/mahogany
    # NOTE: awesome-wm and calla https://github.com/Stardust-kyun/calla
  };

  allModules = {
    imports = [
      command-line.atuin
      command-line.direnv
      command-line.fd
      command-line.fzf
      command-line.git
      command-line.nh
      command-line.nurl
      command-line.pandoc
      command-line.poppler
      command-line.ripgrep

      desktop.firefox
      desktop.foot
      desktop.rofi

      editor.emacs
      editor.neovim

      shell.bash
      shell.fish
      shell.starship
      shell.zsh

      style.fonts
      style.stylix

      window-manager.sway
    ];
  };
}
