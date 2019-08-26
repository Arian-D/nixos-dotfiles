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
      "${builtins.fetchTarball https://github.com/rycee/home-manager/archive/master.tar.gz}/nixos"
    ];

  # Use the systemd-boot EFI boot loader.
  boot = {
    cleanTmpDir = true;
    loader = {
      systemd-boot.enable       = true;
      efi.canTouchEfiVariables  = true;
    };
  };

  hardware.cpu.intel.updateMicrocode = true;

  documentation.dev.enable = true;
  
  environment.variables = {
    OPENSSL_DIR = "${pkgs.openssl.dev}";
    OPENSSL_LIB_DIR = "${pkgs.openssl.out}/lib";
    EDITOR = "nvim";
  };

  # zsh
  programs.zsh = {
    enable                    = true;
    enableCompletion          = true;
    autosuggestions.enable    = true;
    syntaxHighlighting.enable = true;
    promptInit = ''
      bindkey -v
      export PATH=$PATH:~/.npm/bin
    '';

    ohMyZsh = {
      enable = true;
      plugins = [ 
        "git"
        "git-extras"
        "python"
        "cabal"
        "man"
        "sudo"
      ];
      theme = "agnoster";
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
      "vboxuser" # VirtualBox
      "wireshark" # Wrireshark
      "docker" # Docker
    ];
  };

  system.stateVersion = "19.03"; # Did you read the comment?
}
