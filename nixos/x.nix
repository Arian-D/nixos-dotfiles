{configs, pkgs, ...}:
{
/*
  boot.extraModulePackages = [
    pkgs.linuxPackages.nvidia_x11
  ];

  hardware.opengl.extraPackages = [
    pkgs.libGL_driver
    pkgs.linuxPackages.nvidia_x11.out
  ];
  hardware.opengl.extraPackages32 = [
    pkgs.linuxPackages.nvidia_x11.lib32
  ];

  hardware.nvidia.optimus_prime = {
    enable = true;
    nvidiaBusId = "PCI:2:0:0";
    intelBusId = "PCI:0:2:0";
  };
*/

  services.xserver = {
    enable = true;
    # displayManager.gdm.enable = true;
    # displayManager.gdm.wayland = false;
    displayManager.sddm.enable = true;
    desktopManager = {
      default         = "";
      xterm.enable    = false;
      plasma5.enable  = true;
      gnome3.enable = false;
    };

    windowManager = {
      default = "stumpwm";
      stumpwm.enable = true;
      exwm.enable = true;
      /*
      i3 = {
        enable = true;
        configFile = "/etc/nixos/i3-config";
      };
      xmonad = {
        enable = false;
        enableContribAndExtras = false;
        extraPackages = hpkgs: with hpkgs; [
          xmobar
          xmonad-contrib
          xmonad-extras
          xmonad
        ];
      };
      */
    };

    #videoDrivers = [ "nvidia"];
    #videoDrivers = [ "nvidiaLegacy390" ];
    #videoDrivers = [ "nvidiaLegacy340"x ];
    #videoDrivers = [ "nvidiaLegacy304" ];
    #videoDrivers = [ "nvidiaLegacy173" ];
    libinput.enable = true; # touchpad
  };
  hardware.opengl.driSupport32Bit = true;
}

