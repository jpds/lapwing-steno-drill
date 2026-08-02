import { defineConfig, devices } from "@playwright/test";

export default defineConfig({
  testDir: "e2e",
  fullyParallel: true,
  reporter: "list",
  use: {
    baseURL: "http://localhost:8934",
  },
  projects: [
    {
      name: "chromium",
      use: {
        ...devices["Desktop Chrome"],
        // nixpkgs' playwright-driver packages the regular chromium build
        // more reliably than the newer "headless shell" variant Playwright
        // defaults to; forcing this channel avoids a missing-executable
        // error under `nix flake check`.
        channel: "chromium",
        // Nested sandboxing isn't available inside the Nix build sandbox
        // (used by `nix flake check`), so Chromium needs --no-sandbox there.
        launchOptions: { args: ["--no-sandbox"] },
      },
    },
  ],
  webServer: {
    command: "npm run build && npm run serve",
    url: "http://localhost:8934/index.html",
    reuseExistingServer: !process.env.CI,
    timeout: 60_000,
  },
});
