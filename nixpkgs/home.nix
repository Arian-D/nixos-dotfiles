{ config, pkgs, lib, ... }:

with lib;

let

  gitName = "Arian-D";
  gitEmail = "ArianDehghani@protonmail.com";

  home = getEnv "HOME";

  wallpaper = pkgs.fetchurl {
    url = https://i.redd.it/jx0cehys3yr31.jpg;
    sha256 = "1r03jn6rh12yq9a3g20bfnx7l4isc2x4sya0ry9zb2glrcwm3fsr";
  };


in

{
  imports =
    [
      ./firefox/firefox.nix
      ./emacs.nix
    ];


  networkingPackages = with pkgs; [
    nmap wireshark
    openvpn netcat-gnu
  ];

  home.file.wallpaper.source = wallpaper;
  home.file.".fehbg".source = "${wallpaper}";
  home.packages = with pkgs; [
    tty-clock
    neofetch
    libreoffice
    remmina
    nix-direnv
    higan
    spectacle
    nyxt
    pinentry.qt
    torsocks                    # To be used with my remote server
    gimp-with-plugins
    godot steam
    discord jitsi-meet-electron
    github-cli
    nodePackages.pyright
    rnix-lsp
    # Lisp
    sbcl
    guile
    racket
  ];

  home.sessionVariables = {
    EDITOR = "emacsclient";
    WALLPAPER = "${wallpaper}";
  };
  
  home.file.".stack/config.yaml".text = ''
  allow-newer: true
  nix:
    enable: true
  '';

  services.lorri.enable = true;

  programs = {
    mpv.enable = true;

    direnv = {
      enable = true;
      enableZshIntegration = true;
	  };

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
        background_opacity = 0.9;
      };
    };

    chromium.enable = true;

    htop = {
      enable = true;
      treeView = true;
    };
    
    git = {
      enable = true;
      userName = gitName;
      userEmail = gitEmail;
      ignores = [ "*~" ];
      signing = {
        key = "900AFEBE832C4F80034DF1B811F9FBCB5C496EA8";
        signByDefault = true;
      };
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

    # Notification
    dunst = {
      enable = true;
      settings = {
        global = {
          geometry = "300 x5-30+50";
          frame_color = "#eceff1";
          font = "Droid Sans 24";
        };
        urgency_normal = {
          background = "#37474f";
          foreground = "#eceff1";
          timeout = 10;
        };
      };
		};

    polybar = {
      enable = true;
      script = "polybar mybar &";
      extraConfig = ''
      [bar/mybar]
      modules-right = date
      
      [module/date]
      type = internal/date
      interval = 1.0
      date = %Y-%m-%d %H:%M:%S
      '';
    }; 
  };
}
