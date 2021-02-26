{configs, pkgs, lib, ...}:

let
  strictBlockList = pkgs.fetchurl {
    url = https://raw.githubusercontent.com/StevenBlack/hosts/3.4.1/hosts;
    sha256 = "0rrzp8zwj0cy5z1cp063c4qfqf9qp78z9mrdf7aj10qw26170dbf";
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
