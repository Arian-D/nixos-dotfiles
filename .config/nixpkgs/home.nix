{ config, pkgs, lib, ... }:

with lib;

let

  gitName = "Arian-D";
  gitEmail = "ArianDehghani@protonmail.com";
  home = getEnv "HOME";

  wallpaper = pkgs.fetchurl {
    url = https://i.redd.it/qqgds2i3ueh31.jpg;
    sha256 = "066rnpy0h2pxnzc0hlxylwl8cxvs5zfr4pdap4g7nycz855gfm85";
  };

in

{
  imports =
    [
      # ./firefox/firefox.nix
      ./emacs.nix
    ];

  home.file.wallpaper.source = wallpaper;
  home.packages = with pkgs; [
    vlc
    remmina
    higan
    direnv
    spectacle
    (firefox-unwrapped.override {
      enableOfficialBranding = false;
      crashreporterSupport = false;
      geolocationSupport = false;
      drmSupport = false;
      waylandSupport = false;   # Enable when you add Sway
      webrtcSupport = false;
      privacySupport = true;
      ffmpegSupport = true;
      debugBuild = false;
    })
    (chromium.override {
      useOzone = false;         # Enable when you add Sway
    })
    next
    pinentry.qt
    torsocks                    # To be used with my remote server
  ];

  home.sessionVariables = {
    SWANK = let slime = pkgs.emacs26Packages.slime;
            in
              "${slime}/share/emacs/site-lisp/elpa/slime-${slime.version}/swank-loader.lisp";
    EDITOR = "emacsclient";
    WALLPAPER = "${wallpaper}";
  };
  
  programs = {
    mpv.enable = true;
    zsh = {
      enable = true;
      enableAutosuggestions = true;
      enableCompletion = true;
      defaultKeymap = "emacs";
      oh-my-zsh = {
        enable = true;
        theme = "agnoster";
        plugins = [
          "git"
          "docker"
          "python"
          "cabal"
          "stack"
          "man"
          "sudo"
          "nmap"
        ];
      };
    };

    alacritty = {
      enable = true;
      settings = {
        cursor.style = "Beam";
        font.normal.family = "Iosevka Nerd Font";
        font.size = 10;
      };
    };
    
    htop = {
      enable = true;
      treeView = true;
    };
    
    git = {
      enable = true;
      userName = gitName;
      userEmail = gitEmail;
      ignores = [ "*~" ];
    };
    
    lsd = {
      enable = true;
      enableAliases = true;
    };
    
    bat.enable = true;
    
    rofi = {
      enable = true;
      terminal = "alacritty";
      scrollbar = false;
      location = "center";
      theme = "DarkBlue";
      font = "DejaVu Sans extralight 24";
      extraConfig = "rofi.show-icons: true";
    };
    
    feh.enable = true;
    zathura.enable = true;
    gpg.enable = true;
  };
  
  services = {
    gpg-agent = {
      enable = true;
      extraConfig = ''
      allow-emacs-pinentry
      allow-loopback-pinentry
    '';
    };
  };
}
