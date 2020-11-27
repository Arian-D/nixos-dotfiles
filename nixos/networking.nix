{configs, pkgs, lib, ...}:

let
  strictBlockList = pkgs.fetchurl {
    url = https://raw.githubusercontent.com/StevenBlack/hosts/2.7.10/hosts;
    sha256 = "05jxm3k90ysgigyiaspsiz2g1a0jhk08pccc0f914jh1dqcrswcv";
  };

in

{
  # Essential
  networking = {
    hostName = "somewhere";
    networkmanager = {
      enable = true;
      dns = "none";
    };

    enableIPv6 = false;

    firewall = {
      enable = false;
      allowPing = false;
    };
    
    nameservers = [
      "9.9.9.9"
    ];
    
    # Block adware/malware
    extraHosts = lib.readFile "${strictBlockList}";
  };

  # For wireshark
  environment.variables.SSLKEYLOGFILE = "/home/someone/SSLKEYLOGFILE";
  environment.sessionVariables.SSLKEYLOGFILE = "/home/someone/SSLKEYLOGFILE";

}
