{configs, pkgs, ...}:
{
  nixpkgs.config = {
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
  };

  programs.zmap.enable = true;
  programs.npm.enable = true;

  # Packages
  environment.systemPackages = with pkgs; [
    # Essential
    emacs
    manpages
    wget gparted bat ripgrep
    openssl pkg-config
    usbutils
    # Desktop
    home-manager
    xmobar rofi ranger mplayer
    python3Packages.mps-youtube
    libreoffice
    # pdf
    evince mupdf zathura
    # Eye candy
    pywal gotop tty-clock cli-visualizer lsd
    # LaTeX
    texlive.combined.scheme-full
    # C
    llvm gcc clang gnumake cmake gdb binutils
    # rust & wasm
    rustup emscripten
    # python
    (python3.withPackages(ps: with ps; [
      # Devel
      python-language-server
      pyls-mypy pyls-isort pyls-black
      # Essential
      requests
      virtualenvwrapper
      tkinter
      jupyter
      # Web
      django
      # Data science
      pandas
      numpy
      matplotlib
      pytorch
      # tensorflow
      keras-preprocessing
      # misc
      pygame
    ]))
    # Haskal
    (haskellPackages.ghcWithPackages(hs: with hs; [
      yesod
    ] ++
    (let 
      all-hies = import (fetchTarball "https://github.com/infinisil/all-hies/tarball/master") {};
    in
      [ (all-hies.unstable.selection { selector = p: { inherit (p) ghc865; }; }) ]
      )))
    # Elm
    nodePackages.elm-oracle
    nodePackages.typescript-language-server
    nodePackages.vscode-css-languageserver-bin
    # Networking
    wireshark
    bind
    nmap etherape openvpn protonvpn-cli iptables netcat-gnu
    # misc
    godot
    gimp
    blender
  ];  
}
