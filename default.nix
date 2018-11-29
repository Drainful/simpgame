with import <nixpkgs> {};
{
    simpgame = stdenv.mkDerivation rec {
        name = "simpgame";
        buildInputs = [ncurses];
        LD_LIBRARY_PATH = pkgs.stdenv.lib.makeLibraryPath buildInputs;
    };
}
