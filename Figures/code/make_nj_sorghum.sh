#!/usr/bin/tcsh

conda activate /usr/local/usrapps/maize/sorghum/conda/envs/r_env



set gt=/rsstu/users/r/rrellan/sara/SorghumGEA/data/Lasky2015/kinship_sample/all_chr_10K.hmp.txt


echo "Making NJ tree..."

set TASSEL5=/usr/local/usrapps/maize/tassel-5-standalone/run_pipeline.pl

$TASSEL5 -h $gt \
    -tree Neighbor \
    -treeSaveDistance false \
    -export sorghum_tree.nj.txt