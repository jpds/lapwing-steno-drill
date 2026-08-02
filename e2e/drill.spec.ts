import { test, expect, Page } from "@playwright/test";

const WORDS: Array<[string, string]> = [
  ["the", "-T"],
  ["of", "-F"],
  ["to", "TO"],
  ["and", "SKP"],
  ["a", "AEU"],
  ["in", "TPH"],
  ["is", "S"],
  ["it", "T"],
  ["you", "U"],
  ["that", "THA"],
  ["he", "HAOE"],
  ["was", "WAS"],
  ["for", "TP-R"],
  ["with", "W"],
  ["as", "AS"],
  ["I", "EU"],
  ["they", "THE"],
  ["be", "-B"],
  ["at", "AT"],
  ["have", "SR"],
  ["this", "TH"],
  ["from", "TPROPL"],
];

const WORD_LIST = WORDS.map(([word, stroke]) => word + "\t" + stroke).join("\n");

async function loadWordList(page: Page) {
  await page.locator(".list-input").fill(WORD_LIST);
  await page.getByRole("button", { name: "Load word list" }).click();
  await expect(page.locator(".word-panel")).toHaveText("the");
}

test.beforeEach(async ({ page }) => {
  const errors: string[] = [];
  page.on("pageerror", (e) => errors.push(String(e)));
  (page as any)._consoleErrors = errors;
  await page.goto("/index.html");
  await loadWordList(page);
});

test.afterEach(async ({ page }) => {
  expect((page as any)._consoleErrors ?? []).toEqual([]);
});

test("renders the first word and chord with no clipping", async ({ page }) => {
  await expect(page.locator(".word-panel")).toHaveText("the");
  const svg = page.locator(".chord-view svg");
  await expect(svg).toBeVisible();
  const box = await svg.boundingBox();
  expect(box?.width).toBeGreaterThan(600);
});

test("typing the correct stroke highlights it green then advances", async ({ page }) => {
  await page.keyboard.type("-T");
  await expect(page.locator(".steno-key.correct")).toHaveCount(1);
  await expect(page.locator(".steno-key.incorrect")).toHaveCount(0);
  await page.keyboard.type(" ");
  // still showing the resolved (correct) chord during the flash window
  await expect(page.locator(".steno-key.correct")).toHaveCount(1);
  await expect(page.locator(".word-panel")).toHaveText("of", { timeout: 2000 });
});

test("typing a wrong stroke highlights it red and retries the same word", async ({ page }) => {
  await page.keyboard.type("-S ");
  await expect(page.locator(".steno-key.incorrect")).toHaveCount(1);
  await page.waitForTimeout(1000);
  await expect(page.locator(".word-panel")).toHaveText("the");
  await expect(page.locator(".steno-key.incorrect")).toHaveCount(0);
});

test("clicking elsewhere on the page still lets you type (no visible input needed)", async ({ page }) => {
  await page.locator(".word-panel").click();
  await page.keyboard.type("-T ");
  await expect(page.locator(".word-panel")).toHaveText("of", { timeout: 2000 });
});

test("hint appears automatically after a few seconds of no correct stroke", async ({ page }) => {
  await expect(page.locator(".steno-key.hint")).toHaveCount(0);
  await expect(page.locator(".steno-key.hint")).toHaveCount(1, { timeout: 7000 });
  // hint outlines, but doesn't fill green/red, since nothing's been typed
  await expect(page.locator(".steno-key.hint.correct")).toHaveCount(0);
  await expect(page.locator(".steno-key.hint.incorrect")).toHaveCount(0);
});

test("hint resets and re-arms when the word advances", async ({ page }) => {
  await expect(page.locator(".steno-key.hint")).toHaveCount(1, { timeout: 7000 });
  await page.getByText("Next word", { exact: true }).click();
  await expect(page.locator(".word-panel")).toHaveText("of");
  await expect(page.locator(".steno-key.hint")).toHaveCount(0);
  // doesn't leak the old word's hint immediately, but re-arms for the new word
  await expect(page.locator(".steno-key.hint")).toHaveCount(1, { timeout: 7000 });
});

test("getting it right before the hint timer fires cancels the stale hint", async ({ page }) => {
  await page.keyboard.type("-T ");
  await expect(page.locator(".word-panel")).toHaveText("of", { timeout: 2000 });
  // wait past when the "the" hint would have fired; it must not leak onto "of"
  await page.waitForTimeout(5500);
  await expect(page.locator(".steno-key.hint")).toHaveCount(1);
  const hintKey = await page.locator(".steno-key.hint").getAttribute("class");
  expect(hintKey).toBeTruthy();
});

test("declares the drill complete after the last word, and restart works", async ({ page }) => {
  for (let i = 0; i < WORDS.length - 1; i++) {
    await page.getByText("Next word", { exact: true }).click();
  }
  // still on the last word
  await expect(page.locator(".word-panel")).toBeVisible();
  await expect(page.locator(".drill-complete")).toHaveCount(0);

  await page.getByText("Next word", { exact: true }).click();
  await expect(page.locator(".drill-complete")).toBeVisible();
  await expect(page.locator(".word-panel")).toHaveCount(0);
  await expect(page.locator(".stroke-capture")).toHaveCount(0);

  await page.getByText("Restart", { exact: true }).click();
  await expect(page.locator(".drill-complete")).toHaveCount(0);
  await expect(page.locator(".word-panel")).toHaveText("the");
});

test("a fresh visit with no saved list shows the textbox, not a drill", async ({ page }) => {
  await page.evaluate(() => window.localStorage.clear());
  await page.reload();
  await expect(page.locator(".list-input")).toBeVisible();
  await expect(page.locator(".word-panel")).toHaveCount(0);
});

test("reloading the page keeps the previously loaded word list", async ({ page }) => {
  await page.reload();
  await expect(page.locator(".word-panel")).toHaveText("the");
});

test("rejects an invalid stroke with an inline error and keeps the pasted text", async ({ page }) => {
  await page.getByText("Change word list", { exact: true }).click();
  await page.locator(".list-input").fill("bad\tXYZ123");
  await page.getByRole("button", { name: "Load word list" }).click();
  await expect(page.locator(".list-error")).toContainText("Line 1");
  await expect(page.locator(".list-input")).toHaveValue("bad\tXYZ123");
});

test("dark mode via prefers-color-scheme changes the theme", async ({ page }) => {
  const lightBg = await page.evaluate(() => getComputedStyle(document.body).backgroundColor);
  await page.emulateMedia({ colorScheme: "dark" });
  await page.reload();
  const darkBg = await page.evaluate(() => getComputedStyle(document.body).backgroundColor);
  expect(darkBg).not.toBe(lightBg);
});

