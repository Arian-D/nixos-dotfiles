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

in

{
  # Packages
  environment.systemPackages = with pkgs; [
    nixops
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
    # QEMU
    libvirtd.enable = true;
  };
  programs.firejail.enable = true;
  programs.java.enable = true;
}
