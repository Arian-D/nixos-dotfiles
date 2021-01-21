{configs, pkgs, lib, ...}:

let
  strictBlockList = pkgs.fetchurl {
    url = https://raw.githubusercontent.com/StevenBlack/hosts/3.2.11/hosts;
    sha256 = "005kpy368rvyx0drd88kag4wp9jnxjxik2w4c97w7pwgsqi2x8a4";
  };

in

{
  networking = {
    hostName = "somewhere";
    networkmanager = {
      enable = true;
      dns = "none";
    };

    enableIPv6 = false;

    firewall = {
      enable = true;
      allowPing = false;
    };
    
    nameservers = [
      "9.9.9.9"
      "1.1.1.1"
    ];
    
    extraHosts = lib.readFile "${strictBlockList}";
  };

  # For wireshark
  environment.variables.SSLKEYLOGFILE = "/home/someone/SSLKEYLOGFILE";
  environment.sessionVariables.SSLKEYLOGFILE = "/home/someone/SSLKEYLOGFILE";

  programs.wireshark = {
    enable = true;
    package = pkgs.wireshark;
  };

}
