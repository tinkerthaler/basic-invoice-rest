cabal configure && cabal build && cabal run rest-example-happstack &

curl -H "Content-Type: application/json" localhost:3000/1.0/post

shell.nix
let 
  ...
in pkgs.myEnvFun {
  buildInputs = [
    ...
    pkgs.python
    pkgs.libxml2
  ];
 ...
 }

