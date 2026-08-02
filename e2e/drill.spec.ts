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

const STROKE_BY_WORD = new Map(WORDS);

const WORD_LIST = WORDS.map(([word, stroke]) => word + "\t" + stroke).join("\n");

async function loadWordList(page: Page) {
  await page.locator(".list-input").fill(WORD_LIST);
  await page.getByRole("button", { name: "Load word list" }).click();
  await expect(page.locator(".word-panel")).toBeVisible();
}

async function currentWord(page: Page): Promise<string> {
  return (await page.locator(".word-panel").textContent()) ?? "";
}

async function currentStroke(page: Page): Promise<string> {
  const word = await currentWord(page);
  const stroke = STROKE_BY_WORD.get(word);
  if (!stroke) throw new Error("no fixture stroke for word " + JSON.stringify(word));
  return stroke;
}

// | Removing the last key from a multi-key stroke always changes the
// | pressed key set, so this is guaranteed wrong for any stroke with 2+
// | keys. Only used where the caller has already checked for that.
function droppedLastKey(correct: string): string {
  return correct.slice(0, -1);
}

// | "Z" (the rightmost right-hand key) never appears in this fixture's
// | strokes, so typing it alone is guaranteed to be a wrong stroke
// | regardless of which word is currently showing.
const A_WRONG_STROKE = "Z";

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

test("renders a word and chord with no clipping, and shows 1 / N progress", async ({ page }) => {
  await expect(page.locator(".word-panel")).toBeVisible();
  await expect(page.locator(".drill-progress")).toHaveText("1 / " + WORDS.length);
  const svg = page.locator(".chord-view svg");
  await expect(svg).toBeVisible();
  const box = await svg.boundingBox();
  expect(box?.width).toBeGreaterThan(600);
});

test("typing the correct stroke highlights it green, advances, and the progress counter increments", async ({ page }) => {
  const stroke = await currentStroke(page);
  const word = await currentWord(page);
  await page.keyboard.type(stroke);
  await expect(page.locator(".steno-key.correct")).not.toHaveCount(0);
  await expect(page.locator(".steno-key.incorrect")).toHaveCount(0);
  await page.keyboard.type(" ");
  // still showing the resolved (correct) chord during the flash window
  await expect(page.locator(".steno-key.correct")).not.toHaveCount(0);
  await expect(page.locator(".word-panel")).not.toHaveText(word, { timeout: 2000 });
  await expect(page.locator(".drill-progress")).toHaveText("2 / " + WORDS.length);
});

test("typing a wrong stroke highlights it red and can be corrected with no delay", async ({ page }) => {
  const stroke = await currentStroke(page);
  const word = await currentWord(page);
  await page.keyboard.type(A_WRONG_STROKE + " ");
  await expect(page.locator(".steno-key.incorrect")).not.toHaveCount(0);
  await expect(page.locator(".word-panel")).toHaveText(word);
  // no lockout: the wrong attempt's red highlight clears the moment a new
  // stroke starts, and the word advances as soon as the correction lands
  await page.keyboard.type(stroke + " ");
  await expect(page.locator(".word-panel")).not.toHaveText(word, { timeout: 2000 });
});

test("a wrong stroke's missing keys are outlined red, and finishing the stroke corrects it immediately", async ({ page }) => {
  // find a word whose stroke has more than one key, so leaving one out is possible
  let stroke = await currentStroke(page);
  for (let i = 0; i < WORDS.length && stroke.replace("-", "").length < 2; i++) {
    await page.getByText("Next word", { exact: true }).click();
    stroke = await currentStroke(page);
  }
  expect(stroke.replace("-", "").length).toBeGreaterThanOrEqual(2);
  const word = await currentWord(page);
  const partial = droppedLastKey(stroke);

  await page.keyboard.type(partial + " ");
  await expect(page.locator(".steno-key.correct")).toHaveCount(partial.replace("-", "").length);
  await expect(page.locator(".steno-key.incorrect")).toHaveCount(0);
  // the missing key(s) are outlined red without being filled green/red
  await expect(page.locator(".steno-key.missing")).not.toHaveCount(0);
  await expect(page.locator(".steno-key.missing.correct")).toHaveCount(0);
  await expect(page.locator(".steno-key.missing.incorrect")).toHaveCount(0);

  // no artificial delay: typing the full stroke right away clears the red
  // outline and advances past this word
  await page.keyboard.type(stroke + " ");
  await expect(page.locator(".word-panel")).not.toHaveText(word, { timeout: 2000 });
  await expect(page.locator(".steno-key.missing")).toHaveCount(0);
});

test("a missing key stays a plain static red until the hint also fires, then animates between red and amber", async ({ page }) => {
  let stroke = await currentStroke(page);
  for (let i = 0; i < WORDS.length && stroke.replace("-", "").length < 2; i++) {
    await page.getByText("Next word", { exact: true }).click();
    stroke = await currentStroke(page);
  }
  expect(stroke.replace("-", "").length).toBeGreaterThanOrEqual(2);

  await page.keyboard.type(droppedLastKey(stroke) + " ");
  await expect(page.locator(".steno-key.missing")).not.toHaveCount(0);

  const missingKey = () => page.locator(".steno-key.missing").first();
  const missingKeyStroke = () => missingKey().evaluate((el) => getComputedStyle(el).stroke);
  const missingKeyAnimation = () => missingKey().evaluate((el) => getComputedStyle(el).animationName);

  // before the hint timer fires, a missing key on its own is static -
  // wait past the base .steno-key CSS transition (150ms) so the color
  // has settled before sampling, rather than catching it mid-fade
  await page.waitForTimeout(300);
  expect(await missingKeyAnimation()).toBe("none");
  const staticColor = await missingKeyStroke();
  await page.waitForTimeout(500);
  expect(await missingKeyStroke()).toBe(staticColor);

  // once the hint also fires for this word, the same key picks up both
  // classes and starts alternating
  await expect(page.locator(".steno-key.hint")).not.toHaveCount(0, { timeout: 7000 });
  await expect(missingKey()).toHaveClass(/\bhint\b/);
  expect(await missingKeyAnimation()).toBe("missing-alternate");

  // sample across more than one full 4s cycle - two samples exactly half a
  // period apart can coincidentally land on the same interpolated color,
  // so this checks for *any* variation rather than a single before/after pair
  const seenColors = new Set<string>();
  for (let i = 0; i < 10; i++) {
    seenColors.add(await missingKeyStroke());
    await page.waitForTimeout(500);
  }
  expect(seenColors.size).toBeGreaterThan(1);
});

test("clicking elsewhere on the page still lets you type (no visible input needed)", async ({ page }) => {
  const stroke = await currentStroke(page);
  const word = await currentWord(page);
  await page.locator(".word-panel").click();
  await page.keyboard.type(stroke + " ");
  await expect(page.locator(".word-panel")).not.toHaveText(word, { timeout: 2000 });
});

test("hint appears automatically after a few seconds of no correct stroke", async ({ page }) => {
  await expect(page.locator(".steno-key.hint")).toHaveCount(0);
  await expect(page.locator(".steno-key.hint")).not.toHaveCount(0, { timeout: 7000 });
  // hint outlines, but doesn't fill green/red, since nothing's been typed
  await expect(page.locator(".steno-key.hint.correct")).toHaveCount(0);
  await expect(page.locator(".steno-key.hint.incorrect")).toHaveCount(0);
});

test("hint resets and re-arms when the word advances", async ({ page }) => {
  await expect(page.locator(".steno-key.hint")).not.toHaveCount(0, { timeout: 7000 });
  const word = await currentWord(page);
  await page.getByText("Next word", { exact: true }).click();
  await expect(page.locator(".word-panel")).not.toHaveText(word);
  await expect(page.locator(".steno-key.hint")).toHaveCount(0);
  // doesn't leak the old word's hint immediately, but re-arms for the new word
  await expect(page.locator(".steno-key.hint")).not.toHaveCount(0, { timeout: 7000 });
});

test("getting it right before the hint timer fires cancels the stale hint", async ({ page }) => {
  const stroke = await currentStroke(page);
  const word = await currentWord(page);
  await page.keyboard.type(stroke + " ");
  await expect(page.locator(".word-panel")).not.toHaveText(word, { timeout: 2000 });
  // wait past when the old word's hint would have fired; it must not leak onto the new word
  await page.waitForTimeout(5500);
  await expect(page.locator(".steno-key.hint")).not.toHaveCount(0);
  const hintKey = await page.locator(".steno-key.hint").first().getAttribute("class");
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
  await expect(page.locator(".drill-progress")).toHaveText("1 / " + WORDS.length);
});

test("a fresh visit with no saved list shows the textbox, not a drill", async ({ page }) => {
  await page.evaluate(() => window.localStorage.clear());
  await page.reload();
  await expect(page.locator(".list-input")).toBeVisible();
  await expect(page.locator(".word-panel")).toHaveCount(0);
});

test("reloading the page keeps the previously loaded word list", async ({ page }) => {
  await page.reload();
  await expect(page.locator(".word-panel")).toBeVisible();
  await expect(page.locator(".drill-progress")).toHaveText("1 / " + WORDS.length);
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

