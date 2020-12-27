{ configs, pkgs, home-manager, ... }:

{
  nix = {
    package = pkgs.nixUnstable;
    trustedUsers = [ "root" "someone" ];
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ./x.nix
      ./networking.nix
      ./packages.nix
    ];

  boot = {
    kernel.sysctl."vm.swappiness" = 1;
    loader = {
      # systemd-boot.enable = true;
      # systemd-boot.configurationLimit = 10;
      efi = {
        canTouchEfiVariables  = true;
        efiSysMountPoint = "/boot/efi";
      };
      grub = {
        enable = true;
        version = 2;
        efiSupport = true;
        device = "nodev";
      };
    };
  };

  # Long live the battery...
  services.tlp.enable = true;

  # RTFM
  documentation.dev.enable = true;

  fonts.fonts = with pkgs; [
    (nerdfonts.override {
      fonts = [
        "FantasqueSansMono"
        "Iosevka"
        "IBMPlexMono"
      ];
    })
    fantasque-sans-mono
    etBook
  ];

  # Set your time zone.
  time.timeZone = "US/Pacific";

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull;
    support32Bit = true;
  };
  
  # Enable bluetooth
  hardware.bluetooth = {
    enable = true;
    package = pkgs.bluezFull;
  };

  services.blueman.enable = true;
  
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
}
