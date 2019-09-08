{configs, pkgs, ...}:
{
  # Essential
  networking = {
    hostName = "somewhere";
    networkmanager = {
      enable = true;
    };

    enableIPv6 = false;

    firewall = {
      enable = true;
      allowPing = false;
    };
  };
}
