let pkgs = import ../pkgs.nix;
in
  import "${pkgs.ihaskell}/release.nix" {
    compiler = "ghc844";
    nixpkgs = import pkgs.nixpkgs {};
  }
