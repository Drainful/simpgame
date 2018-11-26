with import <nixpkgs> {};
{
    simpgame = stdenv.mkDerivation {
          name = "simpgame";
          buildInputs = [ sbcl
                          ncurses
                          lispPackages.quicklisp ];

          shellHooks = ''
              emacs --daemon
          '';
    };
}
