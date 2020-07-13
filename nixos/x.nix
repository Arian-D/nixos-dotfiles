{configs, pkgs, ...}:

{
  services.xserver = {
    enable = true;
    displayManager.sddm.enable = true;
    #    displayManager.gdm.nvidiaWayland = true;
    displayManager.defaultSession = "none+stumpwm";
    desktopManager = {
      xterm.enable    = false;
      plasma5.enable  = true;
      gnome3.enable = false;
    };

    windowManager = {
      stumpwm.enable = true;
    };    
    videoDrivers = [ "modesetting" "nvidia" ];
    libinput.enable = true;
  };

  hardware.nvidia.prime = {
    offload.enable = true;
    intelBusId = "PCI:0:2:0";
    nvidiaBusId = "PCI:2:0:0";
  };
  hardware.opengl.driSupport32Bit = true;
}
