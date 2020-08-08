{configs, pkgs, ...}:

let
  essentialPackages = with pkgs; [
    home-manager file
    manpages
    wget gparted ripgrep fzf
    usbutils pciutils
    ffmpeg-full
  ];

  networkingPackages = with pkgs; [
    nmap wireshark etherape
    openvpn protonvpn-cli iptables netcat-gnu
  ];

  desktopPackages = with pkgs; [
    mplayer
    libreoffice
    # Eye candy
    ytop tty-clock
    neofetch
  ];

  devPackages = with pkgs; [    
    texlive.combined.scheme-full
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
    guile
    racket
  ];

  nvidia-offload = pkgs.writeShellScriptBin "nvidia-offload" ''
    export __NV_PRIME_RENDER_OFFLOAD=1
    export __NV_PRIME_RENDER_OFFLOAD_PROVIDER=NVIDIA-G0
    export __GLX_VENDOR_LIBRARY_NAME=nvidia
    export __VK_LAYER_NV_optimus=NVIDIA_only
    exec -a "$0" "$@"
  '';

in

{
  # Packages
  environment.systemPackages = with pkgs; [
    pulseeffects
    nixops
    nvidia-offload
    winePackages.stable
  ]
  ++ essentialPackages
  ++ desktopPackages
  ++ networkingPackages
  ++ devPackages;

  # Wireshark to capture them packets
  programs.wireshark.enable = true;
   
  nixpkgs.config = {
    # Stallman is watching you...
    allowUnfree = true;
    allowBroken = false;
  };
  
  virtualisation = {
    virtualbox.host = {
      enable = true;
      enableHardening = true;
    };
    docker.enable = true;
    libvirtd.enable = true;     # QEMU
  };
  programs.firejail.enable = true;
  programs.java.enable = true;
}
