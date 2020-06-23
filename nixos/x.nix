{configs, pkgs, ...}:
{
  services.xserver = {
    enable = true;
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
     };

    libinput.enable = true; # touchpad
  };
}

