with import <nixpkgs> {};
mkShell rec {
    name = "simpgame";
    buildInputs = [ sbcl ncurses ];
    LD_LIBRARY_PATH = pkgs.stdenv.lib.makeLibraryPath buildInputs;
}
