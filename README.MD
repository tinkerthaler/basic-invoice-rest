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

## Refactoring

Rename inside files: Emacs projectile (C p r). Very powerful (retains case)
Rename files: find ./ -name '*User*' | xargs -I {} rename 'User' 'Customer' {}
