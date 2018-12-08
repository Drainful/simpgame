with import <nixpkgs> {};
{
    simpgame = stdenv.mkDerivation rec {
        name = "simpgame";
        buildInputs = [
                        sbcl
                        ncurses

                        # gamekit
                        libGL
                        xorg.libX11
                        xorg.libXrandr
                        xorg.libXinerama
                        xorg.libXxf86vm
                        xorg.libXcursor

                        # qtools
                        gcc-unwrapped
                        glib
                        freetype
                        fontconfig
                        xorg.libSM
                        xorg.libICE
                        xorg.libXrender
                        xorg.libfontenc
                        xorg.libXext
                        xorg.libX11];
        LD_LIBRARY_PATH = "/run/opengl-driver/lib:/run/opengl-driver-32/lib" + pkgs.stdenv.lib.makeLibraryPath buildInputs;
    };
}

