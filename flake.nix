{
  description = "lapwing-steno-drill dev environment and build";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    purescript-overlay = {
      url = "github:thomashoneyman/purescript-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    mkSpagoDerivation = {
      url = "github:jeslie0/mkSpagoDerivation";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, purescript-overlay, mkSpagoDerivation, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            purescript-overlay.overlays.default
            mkSpagoDerivation.overlays.default
          ];
        };
      in
      {
        packages.default = pkgs.mkSpagoDerivation {
          spagoYaml = ./spago.yaml;
          spagoLock = ./spago.lock;
          src = ./.;
          version = "0.1.0";
          nativeBuildInputs = [ pkgs.purs pkgs.spago-unstable pkgs.esbuild ];
          buildPhase = "spago bundle --bundle-type app --platform browser --outfile dist/app.js";
          installPhase = ''
            mkdir -p $out
            cp -r dist $out/dist
            cp index.html $out/index.html
          '';
        };

        checks.default = pkgs.mkSpagoDerivation {
          spagoYaml = ./spago.yaml;
          spagoLock = ./spago.lock;
          src = ./.;
          version = "0.1.0";
          nativeBuildInputs = [ pkgs.purs pkgs.spago-unstable pkgs.nodejs ];
          buildPhase = "spago test";
          installPhase = "mkdir -p $out";
        };

        apps.default = {
          type = "app";
          program = "${pkgs.writeShellApplication {
            name = "serve-lapwing-steno-drill";
            runtimeInputs = [ pkgs.python3 ];
            text = ''
              cd ${self.packages.${system}.default}
              exec python3 -m http.server "''${PORT:-8000}"
            '';
          }}/bin/serve-lapwing-steno-drill";
        };

        devShells.default = pkgs.mkShell {
          name = "lapwing-steno-drill";
          buildInputs = [
            pkgs.purs
            pkgs.spago-unstable
            pkgs.purs-tidy
            pkgs.nodejs
            pkgs.esbuild
          ];
        };
      });
}
