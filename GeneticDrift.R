##### Load necessary libraries ----------------------------------------------------------------------------

library(tidyverse)
library(grid)
library(gridBase)





##### Create function to define initial conditions --------------------------------------------------------

# Set initial conditions
conditions <- function(N0, A0){
  
  # Initial population size
  N <<- N0
  
  # Initial allele frequencies
  A <<- A0
  a <<- 1 - A
  
  # Randomly-generated initial population
  pop <<- data.frame(a1 = sample(c(rep("A", round(A*N, digits = nchar(N) - 1)),
                                   rep("a", round(a*N, digits = nchar(N) - 1))), 
                                 size = N, replace = FALSE),
                     a2 = sample(c(rep("A", round(A*N, digits = nchar(N) - 1)),
                                   rep("a", round(a*N, digits = nchar(N) - 1))), 
                                 size = N, replace = FALSE))}





##### Create genetic drift function GDFP ------------------------------------------------------------------
 
# Simulation for genetic drift in a fixed population size
# NO NATURAL SELECTION and BIRTH AT REPLACEMENT RATE (i.e. constant population)
sim.gdfp <- function(n){
    
    # Generate random mating order; individuals 1 and 2 mate, 3 and 4, 5 and 6, and so on
    mating.random <- sample(1:N, size = N, replace = FALSE)
    
    # Mate each pair, with 2 offspring being created to replace the parents
    for(i in 1:(N/2)){
      mate1 <- as.vector(t(pop[mating.random[2*i - 1], ]))
      mate2 <- as.vector(t(pop[mating.random[2*i], ]))
      assign(paste0("child", 2*i - 1), c(sample(mate1, size = 1), sample(mate2, size = 1)))
      assign(paste0("child", 2*i), c(sample(mate1, size = 1), sample(mate2, size = 1)))}
    
    # Children become the next generation
    pop <<- as.data.frame(matrix(unlist(mget(paste0("child", 1:N))), nrow = N, byrow = TRUE))
    
    # Calculate new allele frequency
    allele.count <<- length(pop[, 1][pop[, 1] == "A"]) + length(pop[, 2][pop[, 2] == "A"])
    return(allele.count/(2*N))}





##### Create genetic drift function GDCP ------------------------------------------------------------------

# Simulation for genetic drift in a changing population
# NO NATURAL SELECTION and VARYING CLUTCH SIZE (i.e. fluctuating population)
sim.gdcp <- function(n){
  
  # Make sure simulation occurs only when population is nonzero
  if(nrow(pop) != 0){
    
    # Ensure that one individual cannot mate if population is an odd number
    if(nrow(pop) %% 2 != 0){
      pop <<- pop[-sample(c(1:nrow(pop)), size = 1), ]}
    
    # Continue only if population is nonzero after pre-reproductive calculations
    if(nrow(pop) != 0){
      
      # Generate random mating order; individuals 1 and 2 mate, 3 and 4, 5 and 6, and so on
      mating.random <- sample(1:nrow(pop), size = nrow(pop), replace = FALSE)
      
      # Mate each pair, with varying numbers of offspring
      for(i in 1:(nrow(pop)/2)){
        mate1 <- as.vector(t(pop[mating.random[2*i - 1], ]))
        mate2 <- as.vector(t(pop[mating.random[2*i], ]))
        nchild <- sample(c(0:4), size = 1, prob = c(0.1, 0.2, 0.3, 0.3, 0.1))
        if(i == 1){
          prev.nchild <- 0}
        if(nchild != 0){
          for(j in 1:nchild){
            assign(paste0("child", prev.nchild + 1), c(sample(mate1, size = 1), sample(mate2, size = 1)))
            prev.nchild <- prev.nchild + 1}}}
      
      # Continue only if population is nonzero after post-reproductive calculations
      if(prev.nchild != 0){
        
        # Children become the next generation
        pop <<- as.data.frame(matrix(unlist(mget(paste0("child", 1:prev.nchild))), 
                                     nrow = prev.nchild, byrow = TRUE))
        names(pop) <<- c("a1", "a2")
        
        # Calculate new allele frequency
        allele.count <<- length(pop[, 1][pop[, 1] == "A"]) + length(pop[, 2][pop[, 2] == "A"])
        return(allele.count/(2*nrow(pop)))
        
      } else {pop <<- data.frame(); return(-1)}
      
    } else {return(-1)}
    
  } else {return(-1)}}





##### Create natural selection function NSCP --------------------------------------------------------------
  
# Simulation for genetic drift and natural selection in a changing population 
# NATURAL SELECTION and VARYING CLUTCH SIZE (i.e. fluctuating population)  
sim.nscp <- function(n){
  
  # Make sure simulation occurs only when population is nonzero
  if(nrow(pop) != 0){
    
    # Natural selection; survival of individuals depends on frequency of dominant allele
    for(i in 1:nrow(pop)){
      pop$surv[i] <<- ifelse(pop$a1[i] == "A" && pop$a2[i] == "A", 
                             sample(c(0, 1), size = 1, prob = c(0.1, 0.9)),
                             ifelse((pop$a1[i] == "A" && pop$a2[i] == "a") || (pop$a1[i] == "a" && pop$a2[i] == "A"), 
                                    sample(c(0, 1), size = 1, prob = c(0.2, 0.8)),
                                    sample(c(0, 1), size = 1, prob = c(0.3, 0.7))))}
    
    # Kill off individuals for which surv = 0, then remove column since it is no longer necessary                                 
    pop %>% filter(surv == 1) %>% 
      select(-surv) ->> pop
    
    # Ensure that one individual cannot mate if population is an odd number
    if(nrow(pop) %% 2 != 0){
      pop <<- pop[-sample(c(1:nrow(pop)), size = 1), ]}
    
    # Continue only if population is nonzero after pre-reproductive calculations
    if(nrow(pop) != 0){
      
      # Generate random mating order; individuals 1 and 2 mate, 3 and 4, 5 and 6, and so on
      mating.random <- sample(1:nrow(pop), size = nrow(pop), replace = FALSE)
      
      # Mate each pair, with varying numbers of offspring
      for(i in 1:(nrow(pop)/2)){
        mate1 <- as.vector(t(pop[mating.random[2*i - 1], ]))
        mate2 <- as.vector(t(pop[mating.random[2*i], ]))
        nchild <- sample(c(0:4), size = 1, prob = c(0.1, 0.2, 0.3, 0.3, 0.1))
        if(i == 1){
          prev.nchild <- 0}
        if(nchild != 0){
          for(j in 1:nchild){
            assign(paste0("child", prev.nchild + 1), c(sample(mate1, size = 1), sample(mate2, size = 1)))
            prev.nchild <- prev.nchild + 1}}}
      
      # Continue only if population is nonzero after post-reproductive calculations
      if(prev.nchild != 0){
        
        # Children become the next generation
        pop <<- as.data.frame(matrix(unlist(mget(paste0("child", 1:prev.nchild))), 
                                     nrow = prev.nchild, byrow = TRUE))
        names(pop) <<- c("a1", "a2")
        
        # Calculate new allele frequency
        allele.count <<- length(pop[, 1][pop[, 1] == "A"]) + length(pop[, 2][pop[, 2] == "A"])
        return(allele.count/(2*nrow(pop)))
        
      } else {pop <<- data.frame(); return(-1)}
      
    } else {return(-1)}
    
  } else {return(-1)}}  





##### Create function to generate graphs ------------------------------------------------------------------

# Function to generate plots
sim.plots <- function(type, ngen, N0, A0){
  
  # Type can be either "gdfp", "gdcp", or "nscp"
  # ngen is number of generations
  # N0 is starting population
  # A0 is starting frequency of dominant allele
  
  # Truncate vectors of allele frequencies if loss, fixation, or population extinction occur
  for(i in 1:5){
    conditions(N0, A0)
    x.val <- c(0:ngen)
    y.val <- c(A, sapply(x.val[1:ngen], get(paste0("sim.", type))))
    if(0 %in% y.val){
      cutoff <- match(0, y.val)[1]
      y.val <- replace(y.val, (cutoff + 1):length(y.val), NA)}
    if(1 %in% y.val){
      cutoff <- match(1, y.val)[1]
      y.val <- replace(y.val, (cutoff + 1):length(y.val), NA)}
    if(-1 %in% y.val){
      cutoff <- match(-1, y.val)[1]
      y.val <- replace(y.val, (cutoff):length(y.val), NA)
      assign(paste0("extinct.", i), TRUE)} else {assign(paste0("extinct.", i), FALSE)}
    x.val <- x.val[1:length(y.val)]
    assign(paste0("y.val.", i), y.val)
    assign(paste0("x.val.", i), x.val)}
  
  # Store data in one place
  data <- as.data.frame(cbind(x.val, as.data.frame(mget(paste0("y.val.", c(1:5))))))
  
  # Choose colours for graphing
  colours <- c("dodgerblue", "firebrick1", "green", "orange", "yellow")
  
  # Set up blank plot
  ggplot() +
    xlab("Number of Generations") +
    ylab(NULL) +
    scale_y_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1), minor_breaks = NULL) +
    theme(panel.grid.major = element_line(colour = "white"), 
          panel.grid.minor = element_line(colour = "white"),
          panel.background = element_rect(fill = "black"),
          plot.background = element_rect(fill = "black"),
          axis.text.x = element_text(colour = "white", size = 23, 
                                     margin = margin(t = 4, r = 0, b = 0, l = 0)),
          axis.text.y = element_text(colour = "white", size = 24, 
                                     margin = margin(t = 0, r = 4, b = 0, l = 0)),
          axis.title.x = element_text(colour = "white", size = 31,
                                      margin = margin(t = 8, r = 0, b = 0, l = 6)),
          axis.title.y = element_text(colour = "white", size = 31,
                                      margin = margin(t = 80, r = 8, b = 0, l = 0)),
          axis.ticks.x = element_line(colour = "white", size = 0.5),
          axis.ticks.y = element_line(colour = "white", size = 0.5),
          axis.ticks.length = unit(8, "points")) -> p
  
  # Graph each set of simulations
  for(i in 1:5){
    
    # Plot the line
    p <- p + geom_line(data = data, aes_string(x = paste0("x.val.", i), y = paste0("y.val.", i)), 
                       size = 1.15, colour = colours[i])
    
    # Place point at time of loss or fixation
    if(0 %in% get(paste0("y.val.", i))){
      temp.df <- data.frame(match(0, get(paste0("y.val.", i))) - 1, 0)
      p <- p + geom_point(data = temp.df, aes_string(x = names(temp.df)[1], y = names(temp.df)[2]), 
                          colour = colours[i], size = 6)}
    if(1 %in% get(paste0("y.val.", i))){
      temp.df <- data.frame(match(1, get(paste0("y.val.", i))) - 1, 1)
      p <- p + geom_point(data = temp.df, aes_string(x = names(temp.df)[1], y = names(temp.df)[2]), 
                          colour = colours[i], size = 6)}
    
    # Place x at time of population extinction
    if(get(paste0("extinct.", i)) == TRUE){
      temp.value <- match(TRUE, is.na(get(paste0("y.val.", i)))) - 2
      temp.df <- data.frame(temp.value, get(paste0("y.val.", i))[temp.value + 1])
      p <- p + geom_point(data = temp.df, aes_string(x = names(temp.df)[1], y = names(temp.df)[2]), 
                          colour = colours[i], size = 6, pch = 13)}}
  
  # Return plot
  return(p)}





##### Visualisation 1: Comparing N0 and A0 in GDFP --------------------------------------------------------

# Prepare graphics device
jpeg(filename = "GDFP.jpeg", width = 2600, height = 1800, units = "px")

# Create blank page
grid.newpage()
plot.new()

# Set grid layout and activate it; create black background
gly <- grid.layout(1800, 2600)
pushViewport(viewport(layout = gly, gp = gpar(fill = "black")))
grid.rect(gp = gpar(lwd = 2, fill = "black", col = "black"))

# Place borders for graphs
pushViewport(viewport(layout.pos.row = 300:950, layout.pos.col = 50:850))
grid.rect(gp = gpar(lwd = 2.5, col = "white", fill = "transparent"))
popViewport()
pushViewport(viewport(layout.pos.row = 300:950, layout.pos.col = 900:1700))
grid.rect(gp = gpar(lwd = 2.5, col = "white", fill = "transparent"))
popViewport()
pushViewport(viewport(layout.pos.row = 300:950, layout.pos.col = 1750:2550))
grid.rect(gp = gpar(lwd = 2.5, col = "white", fill = "transparent"))
popViewport()
pushViewport(viewport(layout.pos.row = 1075:1725, layout.pos.col = 50:850))
grid.rect(gp = gpar(lwd = 2.5, col = "white", fill = "transparent"))
popViewport()
pushViewport(viewport(layout.pos.row = 1075:1725, layout.pos.col = 900:1700))
grid.rect(gp = gpar(lwd = 2.5, col = "white", fill = "transparent"))
popViewport()
pushViewport(viewport(layout.pos.row = 1075:1725, layout.pos.col = 1750:2550))
grid.rect(gp = gpar(lwd = 2.5, col = "white", fill = "transparent"))
popViewport()

# Place top row of graphs: varying A0
print(sim.plots("gdfp", 50, 20, 0.2), vp = viewport(layout.pos.row = 302:948, layout.pos.col = 52:848))
print(sim.plots("gdfp", 50, 20, 0.5), vp = viewport(layout.pos.row = 302:948, layout.pos.col = 902:1698))
print(sim.plots("gdfp", 50, 20, 0.8), vp = viewport(layout.pos.row = 302:948, layout.pos.col = 1752:2548))

# Place bottom row of plots: varying N0
print(sim.plots("gdfp", 50, 10, 0.5), vp = viewport(layout.pos.row = 1077:1723, layout.pos.col = 52:848))
print(sim.plots("gdfp", 50, 100, 0.5), vp = viewport(layout.pos.row = 1077:1723, layout.pos.col = 902:1698))
print(sim.plots("gdfp", 50, 1000, 0.5), vp = viewport(layout.pos.row = 1077:1723, layout.pos.col = 1752:2548))

# Add title text
grid.text(label = c("What affects the strength of genetic drift?"),
          x = 0.018, y = 0.936, just = "left",
          gp = gpar(fontsize = 132, col = "white"))

# Add subtitle text
grid.text(label = c(paste("Random effects can strongly influence the frequency of traits in a population",
                          "and may have significant consequences for genetic diversity.", sep = " "),
                    paste("Here, we explore the effects of population size and initial allele frequency",
                          "on genetic drift in a population whose numbers remain constant.", sep = " ")), 
          x = rep(0.018, 2), y = c(0.880, 0.855), just = "left",
          gp = gpar(fontsize = 39, col = "white", fontface = "italic"))

# Add centre text
grid.text(label = c(paste("Initial allele frequency: Alleles are more likely to be lost at low",
                          "starting frequencies for that allele. On the contrary,",
                          "a high starting frequency increases chances of fixation.", sep = " "),
                    paste("Population size: In small populations, genetic drift is much more",
                          "influential on allele frequency than it is in larger populations.",
                          "In larger populations, the effects are weaker.", sep = " ")), 
          x = rep(0.05, 2), y = c(0.456, 0.422), just = "left",
          gp = gpar(fontsize = 31, col = "white", fontface = "italic"))

# Add centre line to divide top graphs and text from bottom graphs and text
grid.lines(x = c(0.018, 0.982), y = rep(0.438, 2), gp = gpar(col = "white", lty = 5))

# Add arrows clarifying which set of graphs the text corresponds to
grid.polygon(x = c(0.024, 0.030, 0.036), y = c(0.448, 0.463, 0.448), 
             gp = gpar(col = "white", fill = "white"))
grid.polygon(x = c(0.024, 0.030, 0.036), y = c(0.428, 0.413, 0.428), 
             gp = gpar(col = "white", fill = "white"))

# Add background on which graph labels will be placed
grid.rect(x = 0.298, y = 0.818, width = 0.050, height = 0.019,
          gp = gpar(col = "black", fill = "black"))
grid.rect(x = 0.627, y = 0.818, width = 0.050, height = 0.019,
          gp = gpar(col = "black", fill = "black"))
grid.rect(x = 0.953, y = 0.818, width = 0.050, height = 0.019,
          gp = gpar(col = "black", fill = "black"))
grid.rect(x = 0.299, y = 0.385, width = 0.050, height = 0.019,
          gp = gpar(col = "black", fill = "black"))
grid.rect(x = 0.624, y = 0.385, width = 0.054, height = 0.019,
          gp = gpar(col = "black", fill = "black"))
grid.rect(x = 0.947, y = 0.385, width = 0.058, height = 0.019,
          gp = gpar(col = "black", fill = "black"))

# Add labels showing A0 and N0 for the graphs
grid.text(label = bquote("A"[0]*" = 0.2"), x = 0.298, y = 0.818, just = "centre",
          gp = gpar(fontsize = 32, col = "white"))
grid.text(label = bquote("A"[0]*" = 0.5"), x = 0.627, y = 0.818, just = "centre",
          gp = gpar(fontsize = 32, col = "white"))
grid.text(label = bquote("A"[0]*" = 0.8"), x = 0.953, y = 0.818, just = "centre",
          gp = gpar(fontsize = 32, col = "white"))
grid.text(label = bquote("N"[0]*" = 10"), x = 0.299, y = 0.385, just = "centre",
          gp = gpar(fontsize = 32, col = "white"))
grid.text(label = bquote("N"[0]*" = 100"), x = 0.624, y = 0.385, just = "centre",
          gp = gpar(fontsize = 32, col = "white"))
grid.text(label = bquote("N"[0]*" = 1000"), x = 0.947, y = 0.385, just = "centre",
          gp = gpar(fontsize = 32, col = "white"))

# Note that graphics device may vary between computers
# Thus, adjustments may need to be made to this code section

# Deactivate grid layout; finalise graphics save
popViewport()
dev.off()





##### Visualisation 2: Comparing NSCP and GDCP ------------------------------------------------------------

# Prepare graphics device
jpeg(filename = "GDCP-NSCP.jpeg", width = 1800, height = 2000, units = "px")

# Create blank page
grid.newpage()
plot.new()

# Set grid layout and activate it; create black background
gly <- grid.layout(2000, 1800)
pushViewport(viewport(layout = gly, gp = gpar(fill = "black")))
grid.rect(gp = gpar(lwd = 2, fill = "black", col = "black"))

# Place borders for graphs
pushViewport(viewport(layout.pos.row = 350:800, layout.pos.col = 50:875))
grid.rect(gp = gpar(lwd = 2.5, col = "white", fill = "transparent"))
popViewport()
pushViewport(viewport(layout.pos.row = 825:1275, layout.pos.col = 50:875))
grid.rect(gp = gpar(lwd = 2.5, col = "white", fill = "transparent"))
popViewport()
pushViewport(viewport(layout.pos.row = 1300:1750, layout.pos.col = 50:875))
grid.rect(gp = gpar(lwd = 2.5, col = "white", fill = "transparent"))
popViewport()
pushViewport(viewport(layout.pos.row = 350:800, layout.pos.col = 925:1750))
grid.rect(gp = gpar(lwd = 2.5, col = "white", fill = "transparent"))
popViewport()
pushViewport(viewport(layout.pos.row = 825:1275, layout.pos.col = 925:1750))
grid.rect(gp = gpar(lwd = 2.5, col = "white", fill = "transparent"))
popViewport()
pushViewport(viewport(layout.pos.row = 1300:1750, layout.pos.col = 925:1750))
grid.rect(gp = gpar(lwd = 2.5, col = "white", fill = "transparent"))
popViewport()

# Place left column of graphs: GDCP under various starting conditions
print(sim.plots("gdcp", 50, 20, 0.5), vp = viewport(layout.pos.row = 352:798, layout.pos.col = 52:873))
print(sim.plots("gdcp", 50, 300, 0.7), vp = viewport(layout.pos.row = 827:1273, layout.pos.col = 52:873))
print(sim.plots("gdcp", 50, 100, 0.3), vp = viewport(layout.pos.row = 1302:1748, layout.pos.col = 52:873))

# Place right column of graphs: NSCP under same starting conditions as above
print(sim.plots("nscp", 50, 20, 0.5), vp = viewport(layout.pos.row = 352:798, layout.pos.col = 927:1748))
print(sim.plots("nscp", 50, 300, 0.7), vp = viewport(layout.pos.row = 827:1273, layout.pos.col = 927:1748))
print(sim.plots("nscp", 50, 100, 0.3), vp = viewport(layout.pos.row = 1302:1748, layout.pos.col = 927:1748))

# Add title text
grid.text(label = c("Genetic Drift vs. Natural Selection"),
          x = 0.021, y = 0.95, just = "left",
          gp = gpar(fontsize = 114, col = "white"))

# Add subtitle text
grid.text(label = c(paste("Though chance events influence the frequency of traits in a",
                          "population, the strength of natural selection", sep = " "),
                    paste("often exceeds that of genetic drift.",
                          "Here, we demonstrate how natural selection can overcome genetic", sep = " "),
                    paste("drift and lead to the fixation of a beneficial allele in a population",
                          "whose numbers are subject to change.", sep = " ")), 
          x = rep(0.021, 3), y = c(0.900, 0.875, 0.850), just = "left",
          gp = gpar(fontsize = 38, col = "white", fontface = "italic"))

# Add bottom text on left
grid.text(label = c("Genetic drift causes random changes in allele frequencies",
                    "and can lead to loss or fixation of traits; drift effects can be",
                    "very strong when a population starts out small, or when the",
                    "the initial allele frequency is too high or too low."),
          x = c(0.044, 0.029, 0.029, 0.029), y = c(0.112, 0.092, 0.072, 0.052), just = "left",
          gp = gpar(fontsize = 31, col = "white", fontface = "italic"))

# Add bottom text on right
grid.text(label = c("Natural selection can strongly influence allele frequency",
                    "since individuals with beneficial alleles are more likely to",
                    "survive and pass the alleles to their offspring. As a result,",
                    "these beneficial alleles can become frequent or even fixed."), 
          x = c(0.532, 0.518, 0.518, 0.518), y = c(0.112, 0.092, 0.072, 0.052), just = "left",
          gp = gpar(fontsize = 31, col = "white", fontface = "italic"))

# Add lines to divide the 3 rows of graphs, making side-by-side comparison easier
grid.lines(x = c(0.018, 0.982), y = rep(0.357, 2), gp = gpar(col = "white", lty = 5))
grid.lines(x = c(0.018, 0.982), y = rep(0.594, 2), gp = gpar(col = "white", lty = 5))

# Add arrows clarifying which set of graphs the text corresponds to
grid.polygon(x = c(0.029, 0.035, 0.041), y = c(0.105, 0.117, 0.105), 
             gp = gpar(col = "white", fill = "white"))
grid.polygon(x = c(0.515, 0.521, 0.527), y = c(0.105, 0.117, 0.105), 
             gp = gpar(col = "white", fill = "white"))

# Add background on which graph labels will be placed
grid.rect(x = 0.408, y = 0.336, width = 0.150, height = 0.019,
          gp = gpar(col = "black", fill = "black"))
grid.rect(x = 0.408, y = 0.575, width = 0.150, height = 0.019,
          gp = gpar(col = "black", fill = "black"))
grid.rect(x = 0.408, y = 0.812, width = 0.140, height = 0.019,
          gp = gpar(col = "black", fill = "black"))
grid.rect(x = 0.892, y = 0.336, width = 0.150, height = 0.019,
          gp = gpar(col = "black", fill = "black"))
grid.rect(x = 0.892, y = 0.575, width = 0.150, height = 0.019,
          gp = gpar(col = "black", fill = "black"))
grid.rect(x = 0.892, y = 0.812, width = 0.140, height = 0.019,
          gp = gpar(col = "black", fill = "black"))

# Add labels showing A0 and N0 for the graphs
grid.text(label = bquote("N"[0]*" = 100, A"[0]*" = 0.3"), x = 0.408, y = 0.336, just = "centre",
          gp = gpar(fontsize = 32, col = "white"))
grid.text(label = bquote("N"[0]*" = 300, A"[0]*" = 0.7"), x = 0.408, y = 0.575, just = "centre",
          gp = gpar(fontsize = 32, col = "white"))
grid.text(label = bquote("N"[0]*" = 20, A"[0]*" = 0.5"), x = 0.408, y = 0.812, just = "centre",
          gp = gpar(fontsize = 32, col = "white"))
grid.text(label = bquote("N"[0]*" = 100, A"[0]*" = 0.3"), x = 0.892, y = 0.336, just = "centre",
          gp = gpar(fontsize = 32, col = "white"))
grid.text(label = bquote("N"[0]*" = 300, A"[0]*" = 0.7"), x = 0.892, y = 0.575, just = "centre",
          gp = gpar(fontsize = 32, col = "white"))
grid.text(label = bquote("N"[0]*" = 20, A"[0]*" = 0.5"), x = 0.892, y = 0.812, just = "centre",
          gp = gpar(fontsize = 32, col = "white"))

# Note that graphics device may vary between computers
# Thus, adjustments may need to be made to this code section

# Deactivate grid layout; finalise graphics save
popViewport()
dev.off()

