{ pkgs ? import <nixpkgs> {} }:

let
  haskellWithXmonad = pkgs.ghc.withPackages (p: with p; [
    xmonad
    xmonad-extras
    xmonad-contrib
  ]);
in

pkgs.mkShell {
  buildInputs = with pkgs; [
    haskellWithXmonad
  ];
}
