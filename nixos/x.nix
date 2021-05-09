{configs, pkgs, ...}:

{
  services.xserver = {
    enable = true;
    displayManager.sddm.enable = true;
    displayManager.defaultSession = "none+xmonad";
    desktopManager = {
      xterm.enable = false;
      plasma5.enable = true;
      gnome3.enable = false;
    };
    
    windowManager = {
      xmonad = {
        enable = true;
        enableContribAndExtras = true;
      };
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
      mako # notification daemon
      alacritty # Alacritty is the default terminal in the config
      dmenu # Dmenu is the default in the config but i recommend wofi since its wayland native
    ];
  };

  hardware.steam-hardware.enable = true;
  hardware.opengl = {
    enable = true;
    driSupport32Bit = true;
    driSupport = true;
  };

  
  # Pretty fades
  services.picom = {
    enable = true;
    backend = "glx";
    fade = true;
    fadeDelta = 3;
  };
}
