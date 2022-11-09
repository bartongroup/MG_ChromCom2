configfile: "config.yaml"

rscript = config['rscript']
ncells = config['ncells']
ntry = config['ntry']
nbatch = config['nbatch']

SAMPLES = config['samples']
BSAMPLES = config['boot_samples']
BATCHES = range(1, nbatch+1)
T0 = [0, 5, 10, 15]


rule all:
    input:
      expand("fits/fits_{sample}_tau1_k1_k2_tau2.rds", sample=SAMPLES),
      expand("bootstraps/boot_{bsample}_{batch}.tsv", bsample=BSAMPLES, batch=BATCHES)


####################################################################

rule fit_all:
    input: "fit_data/{sample}.tsv"
    output: "fits/fits_{sample}_tau1_k1_k2_tau2.rds"
    threads: 12
    log: "logs/fit_{sample}.log"
    shell:
        "{rscript} scripts/fit_chromcom.R --input-file={input} --output-file={output} --ncells={ncells} --ntry={ntry} --ncores={threads} &> {log}"

####################################################################

rule boot_all:
    input: "fit_data/{bsample}.tsv"
    output: "bootstraps/boot_{bsample}_{batch}.tsv"
    threads: 12
    log: "logs/boot_{bsample}_{batch}.log"
    shell:
        "{rscript} scripts/fit_chromcom.R  --input-file={input} --output-file={output} --ncells={ncells} --ntry={ntry} --ncores={threads} --bootstrap &> {log}"

