{ # The git revision here corresponds to the nixpkgs-unstable channel, which at
  # the time of this writing has GHC 8.6.5 as the default compiler (matching the
  # one used by stack.yaml). Use https://howoldis.herokuapp.com/ to determine
  # the current rev.
  pkgs ? import (builtins.fetchTarball "https://github.com/nixos/nixpkgs/archive/ed07403382c.tar.gz") {}
  # Which GHC compiler to use.
  # To determine the list of compilers available run:
  #   nix-env -f "<nixpkgs>" -qaP -A haskell.compiler
, compiler ? "default"
}:
let
  haskellPackages =
    if compiler == "default"
      then pkgs.haskellPackages
      else pkgs.haskell.packages.${compiler};
  fetchGitHubArchive = owner: repo: rev:
    builtins.fetchTarball "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";

  # Cabal source dists should not contain symlinks targeting files outside its
  # directory. We replace such symlinks with their target here.
  unpackSymlinks = hp: pkgs.haskell.lib.overrideCabal hp (drv: {
    postUnpack = ''
      cp --remove-destination ${./README.md} $sourceRoot/README.md
      cp --remove-destination ${./LICENSE} $sourceRoot/LICENSE
    '';
  });

  # Summoner project derivation.
  projectDrv = (haskellPackages.override {
    overrides = self: super: with pkgs.haskell.lib; {
      hello-world = unpackSymlinks (self.callCabal2nix "hello-world" {});
    };
  }).extend (pkgs.haskell.lib.packageSourceOverrides {
    relude = fetchGitHubArchive "kowainik" "relude"
      "55968311244690f5cc8b4484a37a63d988ea2ec4";
    tomland = fetchGitHubArchive "kowainik" "tomland"
      "8ef78e5fcfd7055b1db6402713d27a0aa42a82f8";
    shellmet = fetchGitHubArchive "kowainik" "shellmet"
      "36149eb0eb2b81916a93cdb92f3cb949d2eb9d23";
    optparse-applicative = fetchGitHubArchive "pcapriotti" "optparse-applicative"
      "5478fc16cbd3384c19e17348a17991896c724a3c";
  });

  # Summoner project shell.
  projectShell = projectDrv.shellFor {
    packages = p:
      [         p.container
      ];
    buildInputs =
      [ projectDrv.cabal-install
        # Dev dependencies below:
        projectDrv.ghcid
        projectDrv.hlint
        # Runtime dependencies below;
        pkgs.curl
        pkgs.git
        pkgs.gitAndTools.hub
      ];
  };
in
if pkgs.lib.inNixShell then projectShell else projectDrv