#!/usr/bin/env python3
"""Run isolated renderer samples sequentially, without competing benchmark jobs."""

import argparse
import json
import os
from pathlib import Path
import statistics
import subprocess


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--emacs", default="emacs")
    parser.add_argument("--baseline", type=Path, required=True)
    parser.add_argument("--frontend", type=Path, required=True)
    parser.add_argument("--backend", type=Path, required=True)
    parser.add_argument("--ratex", type=Path, required=True)
    parser.add_argument("--output-dir", type=Path, required=True)
    parser.add_argument("--rounds", type=int, default=3)
    parser.add_argument("--batch", action="store_true", help="backend-only, no GUI")
    args = parser.parse_args()
    root = Path(__file__).resolve().parents[1]
    output_dir = args.output_dir.resolve()
    output_dir.mkdir(parents=True, exist_ok=True)
    results = {provider: [] for provider in ("mathjax", "latex", "ratex")}
    for repeat in range(args.rounds):
        # Rotate order to reduce consistent warm-machine/order bias.
        providers = list(results)
        providers = providers[repeat % 3:] + providers[:repeat % 3]
        for provider in providers:
            output = output_dir / f"{provider}-{repeat + 1}.json"
            env = dict(os.environ, MATH_BENCH_PROVIDER=provider,
                       MATH_BENCH_OUTPUT=str(output),
                       MATH_BENCH_ROOT=str(args.baseline.resolve() if provider == "mathjax" else root),
                       MATH_BENCH_RATEX=str(args.ratex.resolve()),
                       TMPDIR=str(output_dir) + "/")
            command = [args.emacs, "-Q"]
            if args.batch:
                command.append("--batch")
            command += ["-L", str(args.frontend.resolve()),
                        "-L", str(args.backend.resolve()),
                        "-l", str(root / "test/benchmark-math.el")]
            print(f"Running {provider}, sample {repeat + 1}/{args.rounds}", flush=True)
            proc = subprocess.run(command, env=env, cwd=root, capture_output=True,
                                  text=True, timeout=240)
            if proc.returncode:
                raise RuntimeError(f"{provider} failed: {proc.stdout}\n{proc.stderr}\n"
                                   f"{output.read_text() if output.exists() else 'No JSON result'}")
            results[provider].append(json.loads(output.read_text()))

    summary = {}
    for provider, runs in results.items():
        def median(values):
            return round(statistics.median(values), 4)

        record = {
            "first_ms": median(run["backend"]["first"]["total_ms"] for run in runs),
            "first_enqueue_ms": median(run["backend"]["first"]["enqueue_ms"] for run in runs),
            "uncached_ms": median(item["total_ms"] for run in runs for item in run["backend"]["uncached"]),
            "repeat_ms": median(item["total_ms"] for run in runs for item in run["backend"]["repeat"]),
            "burst_30_ms": median(run["backend"]["burst_30"]["total_ms"] for run in runs),
        }
        if not args.batch:
            record.update({
                "open_30_ms": median(run["frontend"]["open_30"]["total_ms"] for run in runs),
                "reopen_30_ms": median(run["frontend"]["reopen_30"]["total_ms"] for run in runs),
                "motion_ms_per_command": median(run["frontend"]["cached_motion_ms_per_command"] for run in runs),
            })
        summary[provider] = record
    text = json.dumps(summary, indent=2)
    (output_dir / "summary.json").write_text(text + "\n")
    print(text)


if __name__ == "__main__":
    main()
