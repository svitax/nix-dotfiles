{
  pkgs,
  inputs,
  outputs,
  ...
}:
{
  imports = [
    outputs.homeManagerModules.allModules
  ];

  browser = {
    qutebrowser.enable = true;
    zen.enable = true;
  };

  # TODO add all of these command-line modules to a single module?
  command-line = {
    # atuin.enable = true;
    buku.enable = true;
    direnv.enable = true;
    # fd.enable = true;
    git.enable = true;
    man.enable = true;
    nh.enable = true;
    # nurl.enable = true;
    # pandoc.enable = true;
    # poppler.enable = true;
    # ripgrep.enable = true;
    zoxide.enable = true;
  };

  # desktop = {
    # firefox.enable = true;
    # foot.enable = true;
    # rofi.enable = true;
  # };

  graphical = {
    anki.enable = true;
    discord.enable = true;
  };

  shell = {
    bash.enable = true;
    # fish.enable = true;
    starship.enable = true;
    # zsh.enable = true;
  };

  # style.stylix.enable = true;

  text-editor = {
    emacs = {
      enable = true;
      defaultEditor = true;
    };
  };

  # window-manager.sway.enable = true;

  # services.ollama = {
  #   enable = true;
  #   acceleration = "rocm";
  #   environmentVariables = {
  #     # Override the LLVM target by getting the version from your GPU like so
  #     # nix-shell -p "rocmPackages.rocminfo" --run "rocminfo" | grep "gfx"
  #     HSA_OVERRIDE_GFX_VERSION = "10.3.1";
  #     HCC_AMDGPU_TARGET = "gfx1031";
  #   };
  # };

  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = "evermind";
  home.homeDirectory = "/home/evermind";

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "24.05"; # Please read the comment before changing.

  # The home.packages option allows you to install Nix packages into your
  # environment.
  home.packages = with pkgs; [
    # # Adds the 'hello' command to your environment. It prints a friendly
    # # "Hello, world!" when run.
    # pkgs.hello
    # TODO extract home.packages into modules
    python3Full
    mpv
    yt-dlp
    runelite

    # # It is sometimes useful to fine-tune packages, for example, by applying
    # # overrides. You can do that directly here, just don't forget the
    # # parentheses. Maybe you want to install Nerd Fonts with a limited number of
    # # fonts?
    # (pkgs.nerdfonts.override { fonts = [ "FantasqueSansMono" ]; })

    # # You can also create simple shell scripts directly inside your
    # # configuration. For example, this adds a command 'my-hello' to your
    # # environment:
    # (pkgs.writeShellScriptBin "my-hello" ''
    #   echo "Hello, ${config.home.username}!"
    # '')
  ];

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  home.file = {
    # # Building this configuration will create a copy of 'dotfiles/screenrc' in
    # # the Nix store. Activating the configuration will then make '~/.screenrc' a
    # # symlink to the Nix store copy.
    # ".screenrc".source = dotfiles/screenrc;

    # # You can also set the file content immediately.
    # ".gradle/gradle.properties".text = ''
    #   org.gradle.console=verbose
    #   org.gradle.daemon.idletimeout=3600000
    # '';
  };

  # Home Manager can also manage your environment variables through
  # 'home.sessionVariables'. These will be explicitly sourced when using a
  # shell provided by Home Manager. If you don't want to manage your shell
  # through Home Manager then you have to manually source 'hm-session-vars.sh'
  # located at either
  #
  #  ~/.nix-profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  ~/.local/state/nix/profiles/profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  /etc/profiles/per-user/evermind/etc/profile.d/hm-session-vars.sh
  #
  home.sessionVariables = {
    # EDITOR = "emacs";
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Allow unfree packages.
  # There is a known bug in HM about the fact that "allowUnfree = true;"
  # is broken when using HM standalone, so we need "allowUnfreePredicate =
  # true;"
  nixpkgs.config.allowUnfreePredicate = _: true;
}
