with import <nixpkgs> {};
{
    simpgame = stdenv.mkDerivation {
          name = "simpgame";
          buildInputs = [ sbcl
                          ncurses
                          lispPackages.quicklisp
                          lispPackages.quicklisp-to-nix ];

          shellHooks = ''
              emacs --daemon
              quicklisp-to-nix .   
          '';
    };
}
