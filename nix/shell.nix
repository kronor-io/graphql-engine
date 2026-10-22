{ pkgs, project }:
let
  versions = pkgs.lib.importJSON ../server/VERSIONS.json;

  # cabal, hlint and ormolu need not be built with the engine's GHC: build them
  # with the GHC and index-state kronor-io/kronor uses, which are in the Kronor
  # binary cache.
  toolCompiler = "ghc9124";
  index-state = "2026-09-17T08:24:16Z";

  # The same hlint kronor-io/kronor builds. CI uses the prebuilt 3.6.1 binary
  # from server/VERSIONS.json, but 3.6.1 does not build with GHC >= 9.8 and has
  # no aarch64-linux binary.
  hlint = pkgs.haskell-nix.tool toolCompiler "hlint" {
    version = "3.10";
    inherit index-state;
  };

  cabal = pkgs.haskell-nix.tool toolCompiler "cabal" {
    version = versions.cabal-install;
    inherit index-state;
  };

  ormolu = pkgs.haskell-nix.tool toolCompiler "ormolu" {
    version = versions.ormolu;
    inherit index-state;
  };

  # Unix ODBC Support
  freetdsWithODBC = pkgs.freetds.override {
    odbcSupport = true;
    unixodbc = unixODBC;
  };

  msodbcsql = pkgs.unixodbcDrivers.msodbcsql18;

  # Verify this works by running `odbcinst -q -d`.
  # The output should be the headings from the odbcinst.ini file.
  # (You can easily see the generated file by running `cat $ODBCINSTINI`.)
  odbcConfiguration = pkgs.writeTextFile {
    name = "odbc-configuration";
    text = ''
      [${msodbcsql.fancyName}]
      Description = ${msodbcsql.meta.description}
      Driver = ${msodbcsql}/${msodbcsql.driver}
    '';
    destination = "/odbcinst.ini";
  };

  unixODBC = pkgs.unixodbc.overrideAttrs (oldAttrs: {
    configureFlags = (oldAttrs.configureFlags or [ ]) ++ [ "--disable-gui" "--sysconfdir=${odbcConfiguration}" ];
  });
in
project.shellFor {
  # Hoogle would build documentation for every dependency.
  withHoogle = false;

  buildInputs = [
    cabal
    hlint
    ormolu

    pkgs.jq
    pkgs.ghciwatch

    # Integration tests.
    pkgs.nodejs
    pkgs.python3
    pkgs.pyright

    # PostgreSQL, Microsoft SQL Server, & MySQL clients.
    freetdsWithODBC
    pkgs.libmysqlclient
    pkgs.mariadb
    pkgs.postgresql
    unixODBC
    msodbcsql

    pkgs.nixpkgs-fmt
    pkgs.shellcheck
    pkgs.go
  ];
}
