{ pkgs, ...}:

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
    sshfs
  ];

  devPackages = with pkgs; [
    # General
    gnumake
    gcc
    github-cli
    sqlite
    pandoc
    # Dot
    graphviz
    # LaTeX
    texlive.combined.scheme-full
    # Nix
    nix-direnv
    rnix-lsp
    # C[++]
    ccls
    # Python
    (python3.withPackages (p: [ p.jupyter p.tkinter]))
    nodePackages.pyright
    # Android
    # android-studio
    # androidsdk_9_0
    swiProlog
    # Haskal
    (haskellPackages.ghcWithPackages(hs: with hs; [
      stack
      hoogle
    ]))
    haskellPackages.haskell-language-server
    # Lisps
    sbcl
    guile
    chez
    racket
    # (J|T)S
    # nodePackages.javascript-typescript-langserver
    nodePackages.typescript-language-server
    nodePackages.npm
    nodePackages.yarn
    nodePackages.typescript
  ];
  
in

{
  imports = [
    ./firefox.nix
  ];

  home.file.wallpaper.source = wallpaper;
  home.packages = with pkgs; [
    pulseeffects-legacy
    tty-clock
    neofetch
    libreoffice
    evince
    remmina
    spectacle
    # ((import (builtins.getFlake "nixpkgs/nixpkgs-unstable") {}).nyxt)
    pinentry.qt
    torsocks                    # To be used with my remote server
    gimp-with-plugins
    jitsi-meet-electron
    element-desktop
    calibre
    unzip
    xclip
    davfs2
    nextcloud-client
    discord
    nyxt
  ]
  ++ networkingPackages
  ++ devPackages;

  home.sessionVariables = {
    EDITOR = "emacsclient";
    WALLPAPER = "${wallpaper}";
  };

  gtk = {
    enable = true;
  };

  programs = {
    emacs = {
      enable = true;
      extraPackages = epkgs: [ epkgs.vterm ];
    };
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
      extraConfig = {
        show-icons = true;
      };
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
        enable-ssh-support
      '';
    };
  };
}
