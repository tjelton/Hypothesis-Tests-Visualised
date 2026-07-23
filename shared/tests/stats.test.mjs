// Node test runner for the shared stats layer. Run with:
//   node shared/tests/stats.test.mjs
// (On machines without Node, use: osascript -l JavaScript shared/tests/run_jxa.js)

import { createRequire } from "module";
import { readFileSync } from "fs";
import { dirname, join } from "path";
import { fileURLToPath } from "url";

const here = dirname(fileURLToPath(import.meta.url));
const require = createRequire(import.meta.url);
const Stats = require(join(here, "..", "js", "stats.js"));
const runStatsChecks = require(join(here, "stats.checks.js"));
const ref = JSON.parse(readFileSync(join(here, "reference_values.json"), "utf8"));

// The sample fixture is embedded in the reference file (self-contained suite).
const hanValues = ref.sample_stats.han_input;

const result = runStatsChecks(Stats, ref, hanValues, (m) => console.error(m));
console.log(`${result.checks - result.failures}/${result.checks} checks passed`);
if (result.failures > 0) process.exit(1);
