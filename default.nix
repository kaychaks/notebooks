let
  fetcher = { owner, repo, rev, sha256, ... }: builtins.fetchTarball {
          inherit sha256;
          url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";
  };

  inherit (import <nixpkgs> {}) lib;

  versions = lib.mapAttrs
           (_:fetcher)
           (builtins.fromJSON (builtins.readFile ./versions.json));



  nixpkgs = import versions.nixpkgs {};

in
  import "${versions.ihaskell}/release.nix" {
         inherit nixpkgs;
         compiler = "ghc865";
         packages = self: with self; [
                  papa
         ];
  }
