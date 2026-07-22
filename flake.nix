# This flake is using tricks described here https://web.archive.org/web/20260702172210/https://ayats.org/blog/no-flake-utils
{
  description = "A Nix-flake-based Elm development environment";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";

  outputs =
    { self, nixpkgs, ... }@inputs:

    let
      forAllSystems =
        function:
        nixpkgs.lib.genAttrs [
          "x86_64-linux"
          "aarch64-linux"
          "x86_64-darwin"
          "aarch64-darwin"
        ] (system: function nixpkgs.legacyPackages.${system});

    in
    {
      devShells = forAllSystems (pkgs: {
        default = pkgs.mkShellNoCC {
          packages = with pkgs.elmPackages; [
            elm
            elm-format
            elm-test-rs
            elm-test
            nodejs
            elm-doc-preview
            elm-review
          ];
        };
      });
    };
}
