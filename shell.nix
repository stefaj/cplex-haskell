with import <nixpkgs> { };

haskell.lib.buildStackProject {
    name = "cplex-haskell";
    ghc = haskell.packages.ghc802.ghc;
    buildInputs = [ ncurses 
                    git 
                    ghc
                    cabal-install  ]; 

    shellHook = ''
      export SSL_CERT_FILE=${cacert}/etc/ssl/certs/ca-bundle.crt
    '';

}

