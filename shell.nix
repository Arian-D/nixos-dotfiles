{pkgs}:

pkgs.mkShell {
  name = "Shelly";
  buildInputs = with pkgs; [ git ];
}
