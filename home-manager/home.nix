{ config, pkgs, lib, ... }:

with lib;

let

  gitName = "Arian-d";
  gitEmail = "ArianDehghani@protonmail";
  home = lib.getEnv "HOME";

in

{
  imports =
    [
      ./neovim/neovim.nix
      ./firefox/firefox.nix
    ];

  home.packages = with pkgs; [
    # Rust
    rustup
    # Haskel linting
    hlint
  ];

  programs.alacritty = {
    enable = true;
    settings = {
      cursor.style = "Beam";
      font.normal.family = "Hack Nerd Font";
      font.size = 9;
      background_opacity = 0.9;
    };
  };

  programs.git = {
    enable = true;
    userName = gitName;
    userEmail = gitEmail;
  };

  programs.gpg.enable = true;

  # When developing
  /*
  programs.home-manager = {
    enable = true;
    path = home + "playground/home-manager";
  };
  */

  # Enable once my PR gets approved
  /*
  programs.calcurse = {
    enable = true;
    general.confirmQuit = false;
    appearance.layout = 2;
  };
  */
}
