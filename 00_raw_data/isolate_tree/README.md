

bakta folders have two important files for this pipeline:

results/bakta/{sampleid}/{sample_id}.faa - this file has the amino acid sequences for each gene. This is what is searched for the hmm
results/bakta/{sampleid}/{sample_id}.ffn - this file has the nucleic acid sequences for each gene. This is what is used for alignment

This pipeline takes an hmm file that has several genes to search, these genes are supposed to be single copy core genes in the query genome.

It searches for those genes in all the input genomes. A gene is considered present in a genome if its evaule is within the top 
hits of that gene across genomes, within 3 MADs.

Each gene that is in over the configured percentage of input genomes will continue on to algnment at the nucleotide level

Then alignments are concatenated and a tree is built using raxml-ng using the specified outgroup from the config files
