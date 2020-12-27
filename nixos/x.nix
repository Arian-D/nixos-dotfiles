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

  hardware.opengl.driSupport32Bit = true;
  
  # Pretty fades
  services.picom = {
    enable = true;
    backend = "glx";
    fade = true;
    fadeDelta = 3;
  };
}
