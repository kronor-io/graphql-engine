# This Nix flake builds the Hasura GraphQL Engine with haskell.nix and provides
# a development shell.
#
# It uses the same haskell.nix, nixpkgs and hackage.nix as kronor-io/kronor,
# whose CI builds the engine and pushes it to the Kronor binary cache, so most
# of the Haskell closure (GHC included) is downloaded rather than built. Keep
# the pins in flake.lock in step with kronor's.
#
# Start the shell with `nix develop`, or by using `direnv` (see .envrc).
{
  description = "Hasura GraphQL Engine";

  nixConfig = {
    extra-substituters = "https://niks3-staging.kronor.io?tls-certificate=/etc/kronor/niks3-read.pem&tls-private-key=/etc/kronor/niks3-read-key.pem";
    extra-trusted-public-keys = "niks3.staging.kronor.io-1:eBIwkSq2ZbTcW8YfV4aOM1vCvk6pgseZwnSVP5kcwRs=";
  };

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/release-26.05";
    haskell-nix.url = "github:kronor-io/haskell.nix/nixpkgs-ghc-601a4d19";
    haskell-nix.inputs.hackage.follows = "hackage-nix";
    haskell-nix.inputs.nixpkgs.follows = "nixpkgs";
    hackage-nix.url = "github:kronor-io/hackage.nix";
    hackage-nix.flake = false;
  };

  outputs =
    { self
    , flake-utils
    , nixpkgs
    , haskell-nix
    , hackage-nix
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      haskellNix = import haskell-nix {
        inherit system;
        sourcesOverride.hackage = hackage-nix;
      };

      pkgs = import nixpkgs {
        inherit system;
        overlays = [
          # As in kronor-io/kronor: libpq comes from here, so this keeps the
          # engine's dependencies identical to the ones in the cache.
          (final: _: { postgresql = final.postgresql_18; })
          haskellNix.overlay
        ];
        config.allowUnfreePredicate = pkg: builtins.elem (pkgs.lib.getName pkg) [
          "msodbcsql18"
        ];
      };

      project = import ./nix/project.nix { inherit pkgs; src = ./.; };
    in
    {
      packages.graphql-engine = project.hsPkgs.graphql-engine.components.exes.graphql-engine;

      formatter = pkgs.nixpkgs-fmt;

      devShells.default = import ./nix/shell.nix { inherit pkgs project; };
    }
    );
}
