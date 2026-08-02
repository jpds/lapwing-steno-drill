# Lapwing Steno Drill

A browser-based drill for practicing Lapwing Theory steno outlines.
Paste a word list (`word<TAB>STROKE` per line, for instance lines from a
[lapwing-for-beginners](https://lapwing.aerick.ca/) practice file) and drill through it with live
chord feedback, auto-revealing hints, and a randomized word order.

## Run the webapp

```shell
nix run
```

## Development

```shell
nix develop          # dev shell: purs, spago, esbuild, playwright
spago test           # unit tests
npm run build        # bundle to dist/app.js
playwright test      # e2e tests (requires a build first)
```

`nix flake check` runs both the unit and end-to-end Playwright checks.

## License

AGPLv3+
