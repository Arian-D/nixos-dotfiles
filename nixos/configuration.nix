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

  # Enable low-level kvm stuff
  boot.extraModprobeConfig = "options kvm_intel nested=1";

  environment.variables = {
    # The provided $EDITOR in services.emacs doesn't have the '-n' flag
    editor = "emacseditor -n";
  };

  # zsh
  programs.zsh = {
    enable                    = true;
    enableCompletion          = true;
    autosuggestions.enable    = true;
    syntaxHighlighting.enable = true;
    # promptInit = ''
    #   # Add color configs
    #   POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(dir vcs)
    #   POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(battery date time vpn_ip)
    #   source ${pkgs.zsh-powerlevel9k}/share/zsh-powerlevel9k/powerlevel9k.zsh-theme
    # ''; 
    ohMyZsh = {
      enable = true;
      theme = "agnoster";
      plugins = [
        "git"
        "git-extras"
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
      "vboxusers" # VirtualBox
      "wireshark" # Wrireshark
      "docker" # Docker
      "fuse" # For sshfs
    ];
  };

  # Pentesting user
  # users.users.pentester = {
  #   initialPassword = "toor";
  #   isNormalUser = true;
  #   extraGroups = [
  #     "wheel"
  #     "wireshark"
  #   ];
  #   packages = with pkgs; [
  #     aircrack-ng
  #     macchanger
  #     reaverwps-t6x
  #     hashcat
  #     metasploit
  #     nmap
  #     busybox
  #   ];              
  # };

  system.stateVersion = "19.09";
}
