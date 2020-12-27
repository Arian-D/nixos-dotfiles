{configs, pkgs, ...}:

let
  essentialPackages = with pkgs; [
    file
    manpages
    wget ripgrep fd fzf
    usbutils pciutils
    ffmpeg
  ];

  # nvidia-offload = pkgs.writeShellScriptBin "nvidia-offload" ''
  #   export __NV_PRIME_RENDER_OFFLOAD=1
  #   export __NV_PRIME_RENDER_OFFLOAD_PROVIDER=NVIDIA-G0
  #   export __GLX_VENDOR_LIBRARY_NAME=nvidia
  #   export __VK_LAYER_NV_optimus=NVIDIA_only
  #   exec -a "$0" "$@"
  # '';

in

{
  # Packages
  environment.systemPackages = with pkgs; [
    nixops
    # nvidia-offload
    winePackages.stable
  ]
  ++ essentialPackages;

  # Wireshark to capture them packets
  programs.wireshark.enable = true;
   
  nixpkgs.config = {
    # Stallman is watching you...
    allowUnfree = true;
    allowBroken = false;
  };
  
  boot.binfmt.emulatedSystems = [ "aarch64-linux" "armv7l-linux" ];
  virtualisation = {
    # virtualbox.host = {
    #   enable = true;
    #   enableHardening = true;
    # };
    docker.enable = true;
    libvirtd.enable = true;     # QEMU
  };
  programs.firejail.enable = true;
  programs.java.enable = true;
}
