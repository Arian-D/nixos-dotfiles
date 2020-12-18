{ pkgs ? import <nixpkgs>, ... }:

{
  programs.firefox = {
    enable = true;
    # package = pkgs.firefox-unwrapped;
    profiles.nix = {
      settings = {
        ## Privacy
        # Telemetry
        "toolkit.telemetry.enabled" = false;
        "app.shield.optoutstudies.enabled" = false;
        # Content blocking
        "browser.contentblocking.category" = "strict";
        # Disable geo tracking
        "browser.search.geoip.url" = "";
        "geo.enabled" = false;
        # Disable pocket
        "extensions.pocket.enabled" = false;
        # Disable firefox account
        "identity.fxaccounts.enabled" = false;
        # Disable history alltogether
        "places.history.enabled" = false;
        "browser.cache.offline.enable" = false;
        "browser.privatebrowsing.autostart" = true;
        # Prevent websites from seeing battery status
        "dom.battery.enabled" = false;
        # Prevent websites from seeing clipboard status
        "dom.event.clipboardevents.enabled" = false;
        # Fingerprinting
        "privacy.firstparty.isolate" = true;
        "privacy.resistFingerprinting" = true;
        "privacy.trackingprotection.fingerprinting.enabled" = true;
        "privacy.trackingprotection.enabled" = true;
        "media.navigator.enabled" = false;
        # Block Google's so-called "safebrowsing"
        "browser.safebrowsing.malware.enabled" = false;
        "browser.safebrowsing.phishing.enabled" = false;
        # Do not send pings
        "browser.send_pings" = false;
        # Disable IPv6
        "network.dns.disableIPv6" = true;

        ## Security
        # Disable webgl
        "webgl.disabled" = true;
        # Disable cryptomining
        "privacy.trackingprotection.cryptomining.enabled" = true;
        # DNS over HTTPS
        # "network.trr.mode" = 2;
        # "network.trr.uri" = "https://mozilla.cloudflare-dns.com/dns-query";
        # "network.trr.bootstrapAddress" = "1.1.1.1"; # Cloudfare

        ## Personal
        # Disable warn on exit
        "browser.tabs.warnOnClose" = false;
        # Spell check
        "layout.spellcheckDefault" = 2;
        # Dark theme
        "extensions.activeThemeID" = "firefox-compact-dark@mozilla.org";
        "devtools.theme" = "dark";
      };
    };
  };
}
