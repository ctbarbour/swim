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
          erlang = pkgs.beam.interpreters.erlang_29;
          beamPkgs = pkgs.beam.packages.erlang_29;
          # rebar3 3.27.0's own test suite triggers OTP 29's
          # warn_export_var_subexpr warning during checkPhase; skip
          # the check since the rebar3 binary itself builds fine.
          rebar3 = beamPkgs.rebar3.overrideAttrs (_: { doCheck = false; });
        in
        {
          default = pkgs.mkShell {
            packages = [
              erlang
              rebar3
            ];
          };
        }
      );
    };
}
