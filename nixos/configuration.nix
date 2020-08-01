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
    ];

  # Use the systemd-boot EFI boot loader.
  boot = {
    cleanTmpDir = true;
    kernel.sysctl."vm.swappiness" = 1;
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

  system.autoUpgrade.channel = "https://nixos.org/channels/nixos-20.03";
  system.stateVersion = "20.03";
}
