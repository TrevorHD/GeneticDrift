# Overview

This program simulates genetic drift and natural selection under a variety of conditions. Condition GDFP (genetic drift in fixed population) explores how genetic drift affects allele frequencies when birth occurs at replacement rate, while GDCP (genetic drift in changing population) allows for variation in birth rates and NSCP (natural selection in changing population) introduces differential survival that increases with how many copies of the beneficial allele an individual has. Parameters that can be easily manipulated include the initial population size, initial allele frequency, and the number of generations over which the simulation occurs. The the distribution of clutch sizes and effects of the beneficial allele on survival can also be changed, but only by modifying the code.

# Files

**GeneticDrift** *(.R)* - R code used to build the genetic drift simulator and create the visualisations below.

**GDFP** *(.jpeg)* - Plots showing how initial allele frequency and initial population size affect genetic drift.

**GDCP-NSCP** *(.jpeg)* - Plots comparing genetic drift to natural selection under the same initial conditions.

# Images

![](https://github.com/TrevorHD/GeneticDrift/blob/master/GDFP.jpeg)

![](https://github.com/TrevorHD/GeneticDrift/blob/master/GDCP-NSCP.jpeg)
