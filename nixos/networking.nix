{configs, pkgs, ...}:
{
  # Essential
  networking = {
    hostName = "somewhere";
    networkmanager = {
      enable = true;
      insertNameservers = [ "1.1.1.1" ];
    };
    enableIPv6 = false;
    firewall = {
      enable = true;
      allowPing = false;
    };
    nameservers = [ "1.1.1.1" ];
  };
}
