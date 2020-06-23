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

    nameservers = [
      "1.1.1.1"
      "8.8.8.8"
      "8.8.4.4"
    ];
  };
}
