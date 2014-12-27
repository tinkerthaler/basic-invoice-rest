cabal configure && cabal build && cabal run rest-example-happstack &

curl -H "Content-Type: application/json" localhost:3000/1.0/invoice

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

## Git

Git diff staged area
git diff --cached

## Codeship build
### Setup
cabal update
cabal install cabal-install
cabal --version
cabal install --only-dependencies
cabal configure && cabal build && cabal install

### Test
cabal run rest-example-happstack &
#./script.sh