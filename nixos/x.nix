{configs, pkgs, ...}:

{
  services.xserver = {
    enable = true;
    displayManager.sddm.enable = true;
    displayManager.defaultSession = "none+xmonad";
    desktopManager = {
      xterm.enable = false;
      plasma5.enable = true;
    };
    videoDrivers = [ "intel" ];
    libinput = {
      enable = true;
      naturalScrolling = true;
    };
  };

  programs.sway = {
    enable = true;
    wrapperFeatures.gtk = true; # so that gtk works properly
    extraPackages = with pkgs; [
      swaylock
      swayidle
      wl-clipboard
      wofi
      mako
      alacritty
      dmenu
      xwayland
      brightnessctl
      sway-contrib.grimshot
    ];
  };

  hardware.steam-hardware.enable = true;
  hardware.opengl = {
    enable = true;
    driSupport32Bit = true;
    driSupport = true;
  };

  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    # Add these when 21.05 comes out
    # alsa.enable = true;
    # alsa.support32Bit = true;
    # pulse.enable = true;
    # jack.enable = true;
  };
}
