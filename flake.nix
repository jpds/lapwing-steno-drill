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

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      purescript-overlay,
      mkSpagoDerivation,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
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
        # `spago build` compiles every module (app and test) once. Both
        # packages.default (bundling) and checks.default (testing) copy
        # this `output/` in before their own spago command, so purs' own
        # incremental compilation sees everything as already up to date
        # instead of each recompiling the whole project from scratch.
        packages.compiled = pkgs.mkSpagoDerivation {
          spagoYaml = ./spago.yaml;
          spagoLock = ./spago.lock;
          src = ./.;
          version = "0.1.0";
          nativeBuildInputs = [
            pkgs.purs
            pkgs.spago-unstable
          ];
          buildPhase = "spago build";
          installPhase = ''
            mkdir -p $out
            cp -r output $out/output
          '';
        };

        packages.default = pkgs.mkSpagoDerivation {
          spagoYaml = ./spago.yaml;
          spagoLock = ./spago.lock;
          src = ./.;
          version = "0.1.0";
          nativeBuildInputs = [
            pkgs.purs
            pkgs.spago-unstable
            pkgs.esbuild
          ];
          buildPhase = ''
            cp -r ${self.packages.${system}.compiled}/output output
            chmod -R u+w output
            spago bundle --bundle-type app --platform browser --outfile dist/app.js
          '';
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
          nativeBuildInputs = [
            pkgs.purs
            pkgs.spago-unstable
            pkgs.nodejs
          ];
          buildPhase = ''
            cp -r ${self.packages.${system}.compiled}/output output
            chmod -R u+w output
            spago test
          '';
          installPhase = "mkdir -p $out";
        };

        # Reuses packages.default's build; runs via nixpkgs' own
        # playwright-test CLI so the runner and its browsers can't drift
        # out of revision-sync (npm's @playwright/test vs. nix's browsers
        # did, causing a missing-executable error).
        checks.e2e = pkgs.stdenv.mkDerivation {
          pname = "lapwing-steno-drill-e2e";
          version = "0.1.0";
          src = ./.;
          nativeBuildInputs = [
            pkgs.python3
            pkgs.git
            pkgs.playwright-test
          ];
          buildPhase = ''
            runHook preBuild
            export HOME="$TMPDIR"
            rm -rf dist
            cp -r ${self.packages.${system}.default}/dist dist
            cp -f ${self.packages.${system}.default}/index.html index.html
            python3 -m http.server 8934 &
            server_pid=$!
            trap "kill $server_pid" EXIT
            sleep 1
            playwright test --reporter=line
            runHook postBuild
          '';
          installPhase = "mkdir -p $out";
        };

        apps.default = {
          type = "app";
          program = "${
            pkgs.writeShellApplication {
              name = "serve-lapwing-steno-drill";
              runtimeInputs = [ pkgs.python3 ];
              text = ''
                cd ${self.packages.${system}.default}
                exec python3 -m http.server "''${PORT:-8000}"
              '';
            }
          }/bin/serve-lapwing-steno-drill";
        };

        devShells.default = pkgs.mkShell {
          name = "lapwing-steno-drill";
          buildInputs = [
            pkgs.purs
            pkgs.spago-unstable
            pkgs.purs-tidy
            pkgs.nodejs
            pkgs.esbuild
            # `playwright test` here uses matching pre-wired browsers.
            pkgs.playwright-test
          ];
        };
      }
    );
}
