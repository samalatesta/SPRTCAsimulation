#!/bin/bash


# Project name
#$ -P cbs

#join error and output stream
#$ -j y

#Running the main simulation
#Arguments r1, r2, n
#qsub type1_pearson.qsub 0 0 1000
#qsub type1_pearson.qsub 0 .25 1000
#qsub type1_pearson.qsub 0 .5, 1000
#qsub type1_pearson.qsub 0 .75 1000
#qsub type1_pearson.qsub .25 0 1000
#qsub type1_pearson.qsub .25 .25 1000
#qsub type1_pearson.qsub .25 .5 1000
#qsub type1_pearson.qsub .25 .75 1000
qsub type1_pearson.qsub .5 0 1000
#qsub type1_pearson.qsub .5 .25 1000
#qsub type1_pearson.qsub .5 .5 1000
#qsub type1_pearson.qsub .5 .75 1000
#qsub type1_pearson.qsub .75 0 1000
#qsub type1_pearson.qsub .75 .25 1000
#qsub type1_pearson.qsub .75 .5 1000
#qsub type1_pearson.qsub .75 .75 1000

