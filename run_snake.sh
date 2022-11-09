#!/bin/env bash
 snakemake  --rerun-incomplete --latency-wait 60 --jobs 100 --keep-going \
  --drmaa " -cwd -j y -o cluster.log   -pe smp {threads}"
