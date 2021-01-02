{pkgs, ...}:

let
  
  gitName = "Arian-D";
  gitEmail = "ArianXDehghani@gmail.com";

  wallpaper = pkgs.fetchurl {
    url = https://i.imgur.com/GTdaXyT.jpg;
    sha256 = "1s3g071ic330zp8bb4zmkrxi6s9gapyj9mi18riwlqy6kj93mpws";
  };

  networkingPackages = with pkgs; [
    nmap
    openvpn netcat-gnu
  ];

  devPackages = with pkgs; [    
    gnumake
    texlive.combined.scheme-full
    github-cli
    nix-direnv
    python3
    # Haskal
    (haskellPackages.ghcWithPackages(hs: with hs; [
      stack
      hoogle
    ]))
    # Lisps
    sbcl
    guile
    chez
    racket
    # LSPs
    rnix-lsp
    nodePackages.pyright
    ccls
    haskellPackages.haskell-language-server
  ];
  
in

{
  imports = [
    ./emacs.nix
    ./firefox.nix
  ];

  home.file.wallpaper.source = wallpaper;
  home.packages = with pkgs; [
    pulseeffects
    tty-clock
    neofetch
    libreoffice
    evince
    remmina
    spectacle
    next
    pinentry.qt
    torsocks                    # To be used with my remote server
    gimp-with-plugins
    steam
    jitsi-meet-electron
    discord
    element-desktop
  ]
  ++ networkingPackages
  ++ devPackages;

  home.sessionVariables = {
    EDITOR = "emacsclient";
    WALLPAPER = "${wallpaper}";
  };
  
  home.file.".stack/config.yaml".text = ''
        allow-newer: true
        nix:
          enable: true
        '';

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
  
  systemd.user.startServices = true;
  
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
            [module/battery]
            type = internal/battery

            ; This is useful in case the battery never reports 100% charge
            full-at = 99

            ; Use the following command to list batteries and adapters:
            ; $ ls -1 /sys/class/power_supply/
            battery = BAT0
            adapter = ADP1

            ; If an inotify event haven't been reported in this many
            ; seconds, manually poll for new values.
            ;
            ; Needed as a fallback for systems that don't report events
            ; on sysfs/procfs.
            ;
            ; Disable polling by setting the interval to 0.
            ;
            ; Default: 5
            poll-interval = 5
            '';
    }; 
  };
}
