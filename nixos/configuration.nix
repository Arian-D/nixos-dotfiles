# Edit this configuration file to define what should be installed on
# your system.    Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{configs, pkgs, ...}:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ./x.nix
      ./networking.nix
      ./packages.nix
      # "${builtins.fetchTarball https://github.com/rycee/home-manager/archive/master.tar.gz}/nixos"
    ];

  # Use the systemd-boot EFI boot loader.
  boot = {
    cleanTmpDir = true;
    loader = {
      systemd-boot.enable = true;
      systemd-boot.configurationLimit = 10;
      efi.canTouchEfiVariables  = true;
    };
  };

  # Long live the battery...
  services.tlp.enable = true;

  # Additionl docs
  documentation.dev.enable = true;

  fonts.fonts = with pkgs; [
    nerdfonts
    fantasque-sans-mono
  ];

  environment.variables = {
    # For TLS analysis
    SSLKEYLOGFILE = "/tmp/ssl-keys.log";
  };

  # zsh
  programs.zsh = {
    enable                    = true;
    enableCompletion          = true;
    autosuggestions.enable    = true;
    syntaxHighlighting.enable = true;
    ohMyZsh = {
      enable = true;
      theme = "agnoster";
      plugins = [
        "git"
        "docker"
        "python"
        "cabal"
        "stack"
        "man"
        "sudo"
        "nmap"
      ];
    };
  };

  # Set your time zone.
  time.timeZone = "US/Pacific";

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Enable bluetooth
  hardware.bluetooth.enable = true;

  # Someone
  users.users.someone = {
    initialPassword = "amonadisamonoidinthecategoryofendofunctors";
    isNormalUser = true;
    shell = pkgs.zsh;
    extraGroups = [
      "wheel" # sudo
      "networkmanager" # nmtui
      "dialout"
      "libvirtd"
      "vboxusers" # VirtualBox
      "wireshark" # Wrireshark
      "docker" # Docker
      "fuse" # For sshfs
    ];
  };

  system.stateVersion = "20.03";
}
