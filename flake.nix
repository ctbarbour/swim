{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs = { nixpkgs, ... }:
    let
      forAllSystems = nixpkgs.lib.genAttrs [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
    in
    {
      devShells = forAllSystems (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          erlang = pkgs.beam.interpreters.erlang_28;
          beamPkgs = pkgs.beam.packages.erlang_28;
        in
        {
          default = pkgs.mkShell {
            packages = [
              erlang
              beamPkgs.rebar3
            ];
          };
        }
      );
    };
}
