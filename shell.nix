import ./nix/shell.nix {
  name = "textus";
  devTools = { pkgs }: [
    pkgs.gnumake
    pkgs.sqlite
    pkgs.poppler_utils
    pkgs.okular
    pkgs.pdftk
    pkgs.haskellPackages.mustache
  ];
}
