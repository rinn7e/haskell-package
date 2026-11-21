// SPDX-FileCopyrightText: 2026 Moremi Vannak
//
// SPDX-License-Identifier: MIT

/**
 * Writes the expected outputs of the golden test: what turndown itself (its
 * browser build, in Chromium) makes of each input.
 *
 * Usage, from a folder whose node_modules has `turndown` and `playwright`
 * (or `@playwright/test`):
 *
 *   node <path to>/scripts/golden.mjs
 *
 * The inputs are `test/golden/inputs/<name>.html`. For every other
 * `test/golden/<options>/` folder it reads `options.json` (the
 * TurndownService options) and writes `<name>.md`: for every input in
 * `default`, and in another folder for the inputs it already has a `<name>.md`
 * for (the ones its options change; add an empty one to cover another input).
 */
import fs from 'node:fs'
import { createRequire } from 'node:module'
import path from 'node:path'
import { fileURLToPath } from 'node:url'

const require = createRequire(path.join(process.cwd(), 'noop.js'))
const { chromium } = (() => {
  try {
    return require('playwright')
  } catch {
    return require('@playwright/test')
  }
})()
const turndownBuild = path.join(
  path.dirname(require.resolve('turndown/package.json')),
  'lib/turndown.browser.umd.js',
)

const goldenDir = path.join(
  path.dirname(fileURLToPath(import.meta.url)),
  '../test/golden',
)

const browser = await chromium.launch()
try {
  const page = await browser.newPage()
  await page.setContent('<!doctype html><title>golden</title>')
  await page.addScriptTag({ path: turndownBuild })

  const inputsDir = path.join(goldenDir, 'inputs')
  const inputs = fs.readdirSync(inputsDir).filter((f) => f.endsWith('.html'))
  const sets = fs.readdirSync(goldenDir).filter((set) => set !== 'inputs')

  for (const set of sets.sort()) {
    const dir = path.join(goldenDir, set)
    const options = JSON.parse(fs.readFileSync(path.join(dir, 'options.json'), 'utf8'))
    const files = (
      set === 'default'
        ? inputs
        : inputs.filter((f) => fs.existsSync(path.join(dir, f.replace(/\.html$/, '.md'))))
    ).sort()
    const htmls = files.map((f) => fs.readFileSync(path.join(inputsDir, f), 'utf8'))
    const outputs = await page.evaluate(
      ([opts, inputs]) => {
        const service = new window.TurndownService(opts)
        return inputs.map((html) => service.turndown(html))
      },
      [options, htmls],
    )
    files.forEach((f, i) =>
      fs.writeFileSync(path.join(dir, f.replace(/\.html$/, '.md')), outputs[i]),
    )
    console.log(`${set}: ${files.length} outputs`)
  }
} finally {
  await browser.close()
}
