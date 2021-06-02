# Overview

This program simulates genetic drift and natural selection under three different sets of assumptions. Condition GDFP (genetic drift in fixed population) explores how genetic drift affects allele frequencies when birth occurs at replacement rate, while GDCP (genetic drift in changing population) allows for variation in birth rates and NSCP (natural selection in changing population) introduces differential survival that increases with how many copies of the beneficial allele an individual has. Parameters that can be easily manipulated include the initial population size, initial allele frequency, and the number of generations over which the simulation occurs. The the distribution of clutch sizes and effects of the beneficial allele on survival can also be changed, but only by modifying the code.

<br/>

# Files

**GeneticDrift** *(.R)* - R code used to build the genetic drift simulator and create the visualisations below.

**GDCP-NSCP** *(.jpeg)* - Plots, embedded below, that compare genetic drift to natural selection under the same initial conditions.

**GDFP** *(.jpeg)* - Plots, embedded below, that show how initial allele frequency and initial population size affect genetic drift.

<br/><br/>

![](https://github.com/TrevorHD/GeneticDrift/blob/master/Figures/GDFP.jpeg)

![](https://github.com/TrevorHD/GeneticDrift/blob/master/Figures/GDCP-NSCP.jpeg)
