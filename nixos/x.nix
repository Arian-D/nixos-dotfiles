{configs, pkgs, ...}:
{
  services.xserver = {
    enable = true;
    displayManager.sddm.enable = true;
    desktopManager = {
      default         = "";
      xterm.enable    = false;
      plasma5.enable  = true;
    };

    windowManager = {
      default = "i3";
      i3 = {
        enable = true;
        configFile = "/etc/nixos/i3-config";
      };
      /*
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

    libinput.enable = true; # touchpad
  };
}

