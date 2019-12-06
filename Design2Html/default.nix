let pkgs = import ../pkgs.nix;
in
import "${pkgs.ihaskell}/release.nix" {
  compiler = "ghc864";
  nixpkgs = import pkgs.nixpkgs {};
  packages = self: with self; [
    papa
    lens-aeson
    fingertree
    containers
    free
    comonad
    # rose-trees
    zippers
    mtl
  ];
}
