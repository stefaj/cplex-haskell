with import <nixpkgs> { };

haskell.lib.buildStackProject {
    name = "cplex-haskell";
    ghc = haskell.packages.ghc801.ghc;
    buildInputs = [ ncurses 
                    git 
                    cabal-install  ]; 

    shellHook = ''
      export SSL_CERT_FILE=${cacert}/etc/ssl/certs/ca-bundle.crt
    '';

}

