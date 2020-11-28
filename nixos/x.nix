{configs, pkgs, ...}:

{
  services.xserver = {
    enable = true;
    displayManager.sddm.enable = true;
    displayManager.defaultSession = "none+xmonad";
    desktopManager = {
      xterm.enable = false;
      # plasma5.enable = true;
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
  
  # Pretty fades and blurs
  services.picom = {
    enable = true;
    backend = "glx";
    fade = true;
    fadeDelta = 3;
    settings = {
      blur-strength = 100;
      blur-background = true;
      blur-background-frame = true;
      blur-background-fixed = true;
      blur-kern = "7x7box";
    };
  };
}
