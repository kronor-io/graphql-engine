# The haskell.nix project for the engine.
#
# This mirrors how kronor-io/kronor builds the engine in CI
# (nix/graphql-engine.nix there), with the same haskell.nix, nixpkgs and
# hackage.nix pins (see flake.nix), so that GHC and every dependency whose build
# plan has not changed come from the Kronor binary cache instead of being built.
{ pkgs, src }:
let
  haskellLib = pkgs.haskell-nix.haskellLib;
in
pkgs.haskell-nix.project {
  src = haskellLib.cleanSourceWith {
    name = "graphql-engine-src";
    inherit src;
  };
  compiler-nix-name = "ghc${builtins.replaceStrings ["."] [""] (pkgs.lib.fileContents ../.ghcversion)}";
  cabalProjectFreeze = builtins.readFile ../cabal.project.freeze;
  # postgresql-libpq defaults to finding libpq via pg_config; use pkg-config so
  # haskell.nix wires libpq from nixpkgs.
  cabalProjectLocal = ''
    package postgresql-libpq
      flags: +use-pkg-config
  '';
  modules = [
    {
      # GHC 9.14.1 emits -Wl,--gc-sections to bare ld.bfd during Cabal's GHCi
      # library merge step, and binutils 2.46 rejects the cc-driver -Wl, prefix.
      # Dropping the GHCi library removes that step; dynamic libs still cover
      # Template Haskell.
      enableLibraryForGhci = false;
      enableDeadCodeElimination = true;
    }
    {
      # haskell.nix defaults to leaving the debug info in the binary and the
      # RPATH unshrunk, and the debug info carries a store path reference to
      # ghc itself, which would pull ghc into the runtime closure. These apply
      # to every package, so they must match kronor's for the cache to apply.
      dontStrip = false;
      dontPatchELF = false;
    }
    {
      # hashtables 1.3.1 cbits/defs.h declares `void f();` (empty parens) which
      # gcc-15's C23 default reads as `(void)`, conflicting with the 2-arg
      # definition. Build its cbits under the older C standard.
      packages.hashtables.components.library.ghcOptions = [ "-optc-std=gnu17" ];
    }
    {
      # The server lib embeds src-rsr resources (catalog SQL, per-backend source
      # migrations) via Template Haskell. Upstream's extra-source-files list is
      # stale, so haskell.nix's strict per-component cleaner drops them and the
      # TH splice fails. Pull the whole src-rsr tree in (prefix match).
      packages.graphql-engine.components.library.extraSrcFiles = [ "src-rsr/*" ];
      # mimalloc's static.c unity-build #includes its siblings; c-sources lists
      # only static.c, so pull the whole src/ tree in as extra source files.
      packages.graphql-engine.components.exes.graphql-engine.extraSrcFiles = [
        "../preload-mimalloc/mimalloc/src/*"
      ];
    }
    {
      packages.ci-info.cabal-generator = pkgs.lib.mkForce null;
      packages.ci-info.components.library.extraSrcFiles = [ "res/*" "LICENSE" ];
    }
  ];
  sha256map = {
    "https://github.com/agrafix/Spock"."40d028bfea0e94ca7096c719cd024ca47a46e559" = "15vfd5dim502f1p9r2mrq2zspdk15v1nmx6f45iy0y5xcsc1b2qw";
    "https://github.com/jfischoff/postgres-options.git"."3100f7ca4319748a07a46e2838f4c80f8e3f076a" = "07hb7hmmkgfwmsqss02iqh66kblk0aiz9f96275jcw704crpndk2";
    "https://github.com/TristanCacqueray/cron.git"."5f5b662a1d7abc3951ea5a2a625bbf3e83f7a11a" = "1nj4k532jw3zp4jls93plnizpqma3yj4xcxkc2879gvlws5la591";
    "https://github.com/hasura/kriti-lang.git"."c8fd863d359876af3a0fce47db8a5393dc556667" = "1yskmb1b18y6mrnljr7w0dvmi0iir7s29wn0mkc9bdh6rjxalrv6";
    "https://github.com/hasura/odbc.git"."43b07813d635215b3466824d07890de452920fc2" = "06lv7xc762gd4jfvi4vnrwnflsdasas1pxzkmhh8f0c29pq2dp8a";
    "https://github.com/hasura/ekg-core.git"."df610859603b504494ad770bdbb7053a7f0b9a6c" = "0a2v7v84r2r3amc9mbzhs69risr0zlbsbyb8kchi93n23w58x09l";
    "https://github.com/hasura/libdeflate-hs.git"."0715562e5393acfa5dde66b79c2bfbd05c1828b7" = "1il2jmrxdawm0hpr6kwqx84fs36qif57hxh0m1z97z862x7f1rhn";
    "https://github.com/testcontainers/testcontainers-hs"."fe0d6bbce6ce74d5843a964ac231929ea37025e8" = "1y521m6hbbdnn1qmkx3qyy91gyd8pi65k6gknlsgr9lglxj5smiz";
    "https://github.com/hasura/insert-ordered-containers.git"."cdf4f82bc74b9950cb7e69ef2d0add9a8f397a71" = "0qrglqyja617znvfd3i537j0z2gniil3fmd18pzfg1i5isbf13dy";
  };
  supportHpack = false;
}
