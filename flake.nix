{
  description = "Advent of Code 2025 - Erlang Environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            erlang_27
            rebar3
            git
          ];

          shellHook = ''
            echo "🎄 Advent of Code 2025 - Erlang Environment"
            echo "Erlang: $(erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().' -noshell)"
            echo "Rebar3: $(rebar3 version)"
            echo ""
            echo "Quick Start:"
            echo "  rebar3 compile          # Compile the project"
            echo "  rebar3 shell            # Start Erlang shell with project loaded"
            echo "  aoc2025:solve(1, 1).    # Run Day 1, Part 1 (from shell)"
            echo ""
            printf '\n'
          '';
        };
      }
    );
}
