#!/bin/bash


# Project name
#$ -P cbs

#join error and output stream
#$ -j y

#Running the main simulation
#Arguments A, B, r, n
qsub type1.qsub .5 .5 0 1000
qsub type1.qsub .5 .5 .25 1000
qsub type1.qsub .5 .5 .5 1000
qsub type1.qsub .5 .5 .75 1000
qsub type1.qsub .5 .5 .9 1000
qsub type1.qsub .75 .75 0 1000
qsub type1.qsub .75 .75 .25 1000
qsub type1.qsub .75 .75 .5 1000
qsub type1.qsub .75 .75 .75 1000
qsub type1.qsub .75 .75 .9 1000
qsub type1.qsub .9 .9 0 1000
qsub type1.qsub .9 .9 .25 1000
qsub type1.qsub .9 .9 .5 1000
qsub type1.qsub .9 .9 .75 1000
qsub type1.qsub .9 .9 .9 1000
