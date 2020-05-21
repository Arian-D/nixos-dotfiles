{configs, pkgs, ...}:
let
  essentialPackages = with pkgs; [
    manpages
    wget gparted ripgrep fzf
    usbutils pciutils
  ];

  networkingPackages = with pkgs; [
    nmap etherape
    openvpn protonvpn-cli iptables netcat-gnu
  ];

  desktopPackages = with pkgs; [
    home-manager
    mplayer
    libreoffice
    # Eye candy
    pywal gotop tty-clock cli-visualizer
    neofetch
  ];

  devPackages = with pkgs; [    
    # C
    clang gcc gnumake
    global
    texlive.combined.scheme-full
    # python
    (python3.withPackages(ps: with ps; [
      # Devel
      python-language-server
      pyls-mypy pyls-isort pyls-black
      # Essential
      requests
      tkinter
      jupyter
      numpy
      # misc
      selenium
    ]))
    # Haskal
    (haskellPackages.ghcWithPackages(hs: with hs; [
      stack
      hoogle
      base
      hasktags
      stylish-haskell
    ]))
    # Lisp
    sbcl
    # Web
    nodePackages.typescript-language-server
    nodePackages.vscode-css-languageserver-bin
  ];

  # TODO: Move this to a separate file
  superEmacs = let
    myEmacs = pkgs.emacs; 
    emacsWithPackages = (pkgs.emacsPackages.emacsWithPackages);
  in
    emacsWithPackages (epkgs: (with epkgs.melpaStablePackages; [
      # Essential
      helm                        # Survival
      which-key
      magit                       # Git
      doom-themes                 # For chllenger deep
      doom-modeline
      challenger-deep-theme
      nix-mode                    # Nix
      nixos-options
      company                     # Autocompletion
      # Java
      lsp-mode
      lsp-java
      # Haskal
      haskell-mode
      helm-hoogle
      # Uncommon Lisp
      slime
      slime-company
      geiser
      # Docker
      docker
      dockerfile-mode
      # Org
      org-bullets
      org-pomodoro
      org-trello
      # Misc
      # ytel                      # YouTube
      emms                      # Music
      telega                    # Telegram
      emacsql                   # SQL client
    ] ++ (with epkgs.elpaPackages; [
      auctex
    ])));
in

{

  # Packages
  environment.systemPackages = with pkgs; [
    nixops
    superEmacs
  ]
  ++ essentialPackages
  ++ desktopPackages
  ++ networkingPackages
  ++ devPackages;

  # Wireshark to capture them packets
  programs.wireshark.enable = true;
  
  nixpkgs.config = {
    # Stallman is watching you...
    allowUnfree = false;
    allowBroken = false;
  };
  
  virtualisation = {
    virtualbox.host = {
      enable               = true;
      enableHardening      = true;
      enableExtensionPack  = pkgs.config.allowUnfree;
    };
    docker.enable = true;
    # Enable KVM for QEMU
    kvmgt.enable = false;
    libvirtd.enable = false;
  };

  services.bitlbee.enable = false;
  services.bitlbee.plugins = [ pkgs.bitlbee-discord ];
  programs.java.enable = true;

  # ++ (if pkgs.config.allowUnfree then [
  #   unrar
  # ] else [])
  # ++
  #   (let 
  #     all-hies = import (fetchTarball "https://github.com/infinisil/all-hies/tarball/master") {};
  #   in
  #     [ (all-hies.unstable.selection { selector = p: { inherit (p) ghc865; }; }) ]);
}
