# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{
  pkgs,
  outputs,
  ...
}:

{
  imports = [
    ./hardware-configuration.nix
    outputs.nixosModules.allModules
  ];

  boot.systemd-boot.enable = true;

  desktop-manager.xfce.enable = true;
  # desktop-manager.kde.enable = true;

  nix-settings.enable = true;

  storage.onedrive.enable = true;

  style.fonts.enable = true;

  documentation = {
    enable = true;
    doc.enable = true;
    info.enable = true;
    man.enable = true;
  };

  network.stevenblack.enable = true;

  # TOOD refactor ollama config
  services.ollama = {
    enable = true;
    acceleration = "rocm";
    environmentVariables = {
      # Override the LLVM target by getting the version from your GPU like so
      # nix-shell -p "rocmPackages.rocminfo" --run "rocminfo" | grep "gfx"
      # NOTE 2025-06-12 My RADEON 6700XT is gfx1031, which is not officially
      # supported by AMD in ROCm. See the following link:
      # <https://rocm.docs.amd.com/projects/install-on-linux/en/latest/reference/system-requirements.html>
      HSA_OVERRIDE_GFX_VERSION = "10.3.0";
      HCC_AMDGPU_TARGET = "gfx1031";
    };
    rocmOverrideGfx = "10.3.0";
    loadModels = [
      "deepseek-r1:1.5b"
      "qwen3:1.7b"
      "llama3.2:1b"
      "gemma3:1b"
      "qwen2.5vl:3b"
    ];
  };

  networking.hostName = "erasmus"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "America/New_York";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_US.UTF-8";
    LC_IDENTIFICATION = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_NAME = "en_US.UTF-8";
    LC_NUMERIC = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_TELEPHONE = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
  };

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound with pipewire.
  services.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    #jack.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };

  # Enable bluetooth support.
  hardware.bluetooth.enable = true;
  hardware.bluetooth.powerOnBoot = true; # powers up the default Bluetooth controller on boot

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.evermind = {
    isNormalUser = true;
    description = "evermind";
    extraGroups = [
      "networkmanager"
      "wheel"
    ];
    packages = with pkgs; [
      kdePackages.kate
      #  thunderbird
    ];
  };

  # Install firefox.
  programs.firefox.enable = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    #  vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
    #  wget
    home-manager
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Enable the SSH agent.
  # Starts a user systemd service with the ssh-agent at loging. You can see the
  # service with command `systemctl --user status ssh-agent'.
  #
  # It also provides the environment variable `$SSH_AUTH_SOCK' which refers to
  # /run/user/1000/ssh-agent, in this case for user id 1000.
  #
  # If you want to use a ssh key pair for authenticating, you can add it to the
  # ssh-agent using the command `ssh-add'.
  programs.ssh.startAgent = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.11"; # Did you read the comment?

}
