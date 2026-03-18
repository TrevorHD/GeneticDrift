# Overview

A program simulating genetic drift and natural selection under several different sets of assumptions. Here, we examine:

* How the strength of genetic drift varies based on fixed population size and initial allele frequency
* How natural selection and genetic drift compare across growing populations with identical initial allele frequencies and population sizes

The simulation code allows users to easily change the initial population size, initial allele frequency, and the number of generations over which the simulation occurs in order to explore their effects on genetic drift and natural selection. The distribution of clutch sizes and effects of the beneficial allele on survival can also be changed, but involve additional modifications to the simulation code.

<br/>

# Files

## Figures

**GDPlots1** *(.jpeg)* - Plots showing how initial allele frequency and population size affect genetic drift.

**GDPlots2** *(.jpeg)* - Plots showing how genetic drift and natural selection compare in identical populations.

## Scripts

**GeneticDrift** *(.R)* - Script used to simulate genetic drift and natural selection, as well as to generate figures.

<br/>

# Featured Images

Plots showing how initial allele frequency and population size affect genetic drift. If all other factors are held constant, increasing the population size decreases the likelihood of alleles becoming fixed or lost, while changing the starting frequency can increase chances of loss if close to 0 or increase chances of fixation if close to 1.

<kbd>![](https://github.com/TrevorHD/GeneticDrift/blob/master/Figures/GDPlots1.jpeg)</kbd>

Plots showing how genetic drift and natural selection compare in identical populations. For a given population size and allele starting frequency, natural selection typically has much stronger effects on allele frequency than genetic drift.

<kbd>![](https://github.com/TrevorHD/GeneticDrift/blob/master/Figures/GDPlots2.jpeg)</kbd>
