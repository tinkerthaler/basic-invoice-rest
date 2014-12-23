let 
  pkgs = import <nixpkgs> {};
  haskellPackages = pkgs.haskellPackages.override {
    extension = self: super: {
      rest-example = self.callPackage ./rest-example.nix {};
    };
  };
  name = "rest-example";
in pkgs.myEnvFun {
  buildInputs = [
    (haskellPackages.ghcWithPackagesOld (hs: ([
      hs.cabalInstall
      hs.hscolour
      hs.hoogle
    ]
    ++ hs.rest-example.propagatedNativeBuildInputs
    )))
    pkgs.python
    pkgs.libxml2
  ];
  inherit name;

 }
