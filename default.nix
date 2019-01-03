let
  fetcher = { owner, repo, rev, sha256, ... }: builtins.fetchTarball {
          inherit sha256;
          url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";
  };

  inherit (import <nixpkgs> {}) lib;

  versions = lib.mapAttrs
           (_:fetcher)
           (builtins.fromJSON (builtins.readFile ./versions.json));


  overlay = self: super: {
    python3 = super.python3.override {
      packageOverrides = p-self: p-super: {
        docutils = p-super.docutils.overridePythonAttrs (old: {doCheck = false;});
      };
    };
  };

  nixpkgs = import versions.nixpkgs { overlays = [ overlay ]; };

in
  import "${versions.ihaskell}/release.nix" {
         inherit nixpkgs;
         compiler = "ghc844";
         packages = self: with self; [
                  papa
         ];
  }
