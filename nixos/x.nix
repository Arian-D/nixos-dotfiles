{configs, pkgs, ...}:

{
  services.xserver = {
    enable = true;
    displayManager.sddm.enable = true;
    displayManager.defaultSession = "none+xmonad";
    desktopManager = {
      xterm.enable = false;
      plasma5.enable = false;
      gnome3.enable = false;
    };
    
    windowManager = {
      xmonad = {
        enable = true;
        enableContribAndExtras = true;
      };
    };    

    videoDrivers = [ "intel" ];
    libinput.enable = true;
  };

  # hardware.nvidia.prime = {
  #   offload.enable = true;
  #   intelBusId = "PCI:0:2:0";
  #   nvidiaBusId = "PCI:2:0:0";
  # };

  hardware.opengl.driSupport32Bit = true;
  
  # Pretty fades
  services.picom = {
    enable = true;
    backend = "glx";
    fade = true;
    fadeDelta = 3;
  };
}
