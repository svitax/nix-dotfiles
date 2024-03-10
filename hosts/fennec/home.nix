# This if your home-manager configuration file
# Use this to configure your home environment (it replaces ~/.config/nixpkgs/home.nix)
{ inputs, outputs, pkgs, ... }: {
  # You can import other home-manager modules here
  imports = [
    # If you want to use home-manager modules from other flakes (such as nix-colors):
    # inputs.nix-colors.homeManagerModule
    inputs.nix-colors.homeManagerModules.default
    inputs.nix-index-database.hmModules.nix-index
    # inputs.neovim-flake.nixosModules."x86_64-linux".hm

    # You can also split up your configuration and import pieces of it here:
    ../../home/wm/sway/sway.nix
    ../../home/style/colors.nix
    ../../home/style/fonts.nix

    ../../home/app/browser/firefox.nix
    ../../home/app/browser/qutebrowser.nix

    ../../home/wm/gnome/gnome.nix
    ../../home/app/terminal/kitty/kitty.nix
    ../../home/app/sioyek.nix

    ../../home/shell/bat/bat.nix
    ../../home/app/emacs/emacs.nix
    ../../home/shell/direnv.nix
    ../../home/shell/eza.nix
    ../../home/shell/fish/fish.nix
    ../../home/shell/fzf.nix
    ../../home/shell/ghq.nix
    ../../home/shell/git.nix
    # ../../home/shell/helix/helix.nix
    ../../home/shell/lf/lf.nix
    ../../home/shell/nvim/neovim.nix
    ../../home/shell/mimeapps.nix
    ../../home/shell/papis.nix
    ../../home/shell/starship.nix
    ../../home/shell/tmux.nix
    ../../home/shell/zellij/zellij.nix
    ../../home/shell/zoxide.nix
  ];

  nixpkgs = {
    # You can add overlays here
    overlays = [
      # Add overlays your own flake exports (from overlays and pkgs dir):
      # outputs.overlays.additions
      # outputs.overlays.modifications
      # outputs.overlays.zjstatus

      # You can also add overlays exported from other flakes:
      inputs.neovim-nightly-overlay.overlay
      inputs.emacs-overlay.overlay
      inputs.emacs-overlay.overlays.package
      # inputs.nvim-fennec.overlays.default
    ];
    # Configure your nixpkgs instance
    config = {
      allowUnfree = true;
      # Workaround for https://github.com/nix-community/home-manager/issues/2942
      allowUnfreePredicate = _: true;
    };
  };

  home = {
    username = "svitax";
    homeDirectory = "/home/svitax";
  };

  # Add stuff for your user as you see fit:
  # programs.neovim.enable = true;
  home.packages = with pkgs; [
    # nvim-fennec # nvim-flake output
    # zellij-smart-sessionizer
    # live-grep # workaround for live-grep in helix until provided by core or plugins
    # helix-live-grep # workaround for live-grep in helix until provided by core or plugins
    ripgrep
    fd
    # wl-clipboard # causes weird flickering in neovim + tmux
    xclip
    lazygit
    nurl # prefetch sources for nix build tool
    nix-init # generate nix packages from urls
    atool # extract and archive
    unzip
    texlab
    aspellDicts.en
    aspellDicts.pt_BR
    hunspellDicts.en_US
    hunspellDicts.pt_BR
  ];

  # Enable home-manager
  programs.home-manager.enable = true;

  # Nicely reload system units when changing configs
  systemd.user.startServices = "sd-switch";

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  home.stateVersion = "23.05";
}
