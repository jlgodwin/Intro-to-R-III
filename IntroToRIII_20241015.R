## ----setup, echo = FALSE-----------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_knit$set(root.dir = '~/Dropbox/Workshops/IntrotoRIII/Intro-to-R-III')


## ----knit_ex, eval = FALSE---------------------------------------------------------------------
## data(cars)
## summary(cars)


## ----pressure, echo=FALSE, fig.align = 'center', fig.cap = "For example, this figure was created with `echo = FALSE` and `eval = TRUE`."----
plot(pressure)


## ----eval = F----------------------------------------------------------------------------------
## install.packages('knitr')
## library(knitr)
## purl('IntroToRIII_20231102.Rmd')


## ----warning = F, message = F------------------------------------------------------------------
#install.packages("ggplot2")
#install.packages('palmerpenguins')
library(ggplot2)
library(palmerpenguins)


## ----iris--------------------------------------------------------------------------------------
summary(penguins)


## ----echo=FALSE--------------------------------------------------------------------------------
hist(penguins$flipper_length_mm)


## ----iris sepal, echo=FALSE, tidy =T-----------------------------------------------------------
hist(penguins$flipper_length_mm,
     main = "Histogram of Flipper Length (in mm)",   #main title of the graph
     xlab = "Flipper Length",                #x-axis label (can also use ylab for y-axis label)
     col = "purple4",                      #change color of the bars
     breaks = 45)                          #alter the number of bins


## ----warning = F, message = F, error = F-------------------------------------------------------
ggplot(penguins, aes(x = flipper_length_mm)) +
  geom_histogram()


## ----------------------------------------------------------------------------------------------
ggplot(penguins, aes(x = flipper_length_mm)) +
  geom_histogram(fill = "darkgoldenrod", #alter the fill color of the bars
                 color = "white") +      #alter the outline color of the bars
  ggtitle("Distribution of Flipper Length") +
  xlab("Flipper Length") +
  ylab("Frequency") +
  theme_minimal()     #changes the background theme.  The default is grey.
  


## ----------------------------------------------------------------------------------------------
#Summarize # of penguins from each 
#island in a table
counts <- table(penguins$island)

#Create the plot
barplot(counts) 


## ----------------------------------------------------------------------------------------------
barplot(counts, 
        main = "Island of Origin",
        col = "mediumpurple2",
        ylab = "Frequency")


## ----------------------------------------------------------------------------------------------
ggplot(penguins, aes(x = island)) +
  geom_bar(fill = "darkblue") +
  xlab("Island of Origin") +
  ylab("Frequency") +
  theme_bw()    #note the difference in background with this theme


## ----------------------------------------------------------------------------------------------
#Make a quick summary table of penguins
#by island:
library(dplyr)
library(tidyr)
species_by_island <- penguins %>%
  group_by(species, island) %>%
  tally()

#Graph out the summary table using ggplot2:
ggplot(species_by_island, aes(x = species, y = n, fill = island)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Island of Origin") +
  ylab("Number of individuals of each species") +
  theme_bw()    


## ----------------------------------------------------------------------------------------------
#summarize average flipper length by island
flippers_by_island <- penguins %>%
  group_by(island) %>%
  summarize(mean_flipper_length = mean(flipper_length_mm, na.rm = T))

#graph it out:
ggplot(flippers_by_island, aes(x = island, y = mean_flipper_length, fill = island)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Island of Origin") +
  ylab("Frequency") +
  theme_bw() + 
  theme(legend.position = "none")  +  #use this last line to get rid of the legend 
  scale_fill_manual(values = c("purple4", "darkgoldenrod", "black"))


## ----------------------------------------------------------------------------------------------
plot(penguins$body_mass_g, penguins$flipper_length_mm,
     xlab = "Body mass (in g)",
     ylab = "Flipper Length (in mm)",
     frame = FALSE, #removes the boundary that R automatically puts on a scatterplot
     pch = 19)  #specifies the type of dot


## ----------------------------------------------------------------------------------------------
plot(penguins$flipper_length_mm,
     penguins$body_mass_g,
     xlab = "Flipper length (mm)",
     ylab = "Body Mass (g)",
     main = "",
     pch = 2, col = 'red')


## ----eval = FALSE------------------------------------------------------------------------------
## getwd()
## setwd("~/Dropbox/Workshops/IntrotoRIII/")


## ----eval = FALSE------------------------------------------------------------------------------
## knitr::opts_knit$set(root.dir = "~/Dropbox/Workshops/IntrotoRIII/")


## ----pdf---------------------------------------------------------------------------------------
pdf("Figures/WeightvsHeight.pdf",
    height = 4, width = 4)
plot(penguins$flipper_length_mm,
     penguins$body_mass_g,
     xlab = "Flipper length (mm)",
     ylab = "Body Mass (g)",
     main = "",
     pch = 2, col = 'red')
dev.off()


## ----plot_lims---------------------------------------------------------------------------------
max(penguins$flipper_length_mm)
max(penguins$flipper_length_mm, na.rm = TRUE)

xlimits <- c(min(penguins$flipper_length_mm,
                  na.rm = TRUE)-1,
              max(penguins$flipper_length_mm,
                  na.rm = TRUE)+1)
ylimits <- c(min(penguins$body_mass_g,
                  na.rm = TRUE)-1,
              max(penguins$body_mass_g,
                  na.rm = TRUE)+1)

plot(penguins$flipper_length_mm[penguins$species == "Adelie"],
     penguins$body_mass_g[penguins$species == "Adelie"],
     xlim = xlimits,
     ylim = ylimits,
     xlab = "Flipper length (mm)",
     ylab = "Body Mass (g)",
     pch = 2, col = 'navyblue')
points(penguins$flipper_length_mm[penguins$species == "Chinstrap"],
       penguins$body_mass_g[penguins$species == "Chinstrap"],
       pch = 19, col = 'deeppink',
       lwd = 2)
points(penguins$flipper_length_mm[penguins$species == "Gentoo"],
       penguins$body_mass_g[penguins$species == "Gentoo"],
       pch = 21, col = 'deepskyblue',
       bg = alpha('deepskyblue', 0.5))
legend('topleft', pch = c(2,19,19), 
       lwd = c(1,2,1), lty = c(NA, NA,NA),
       col = c("navyblue",
               "deeppink",
               alpha("deepskyblue",.5)),
       legend = c("Adelie",
                  "Chinstrap",
                  "Gentoo"), bty = 'n')


## ----par_mfrow---------------------------------------------------------------------------------
par(mfrow = c(1,3))
plot(penguins$flipper_length_mm[penguins$species == "Adelie"],
     penguins$body_mass_g[penguins$species == "Adelie"],
     xlim = xlimits,
     ylim = ylimits,
     xlab = "Flipper length (mm)",
     ylab = "Body Mass (g)",
     main = "Adelie",
     pch = 19, col = 'navyblue')
plot(penguins$flipper_length_mm[penguins$species == "Chinstrap"],
     penguins$body_mass_g[penguins$species == "Chinstrap"],
     xlim = xlimits,
     ylim = ylimits,
     xlab = "Flipper length (mm)",
     ylab = "Body Mass (g)",
     main = "Chinstrap",
     pch = 19, col = 'deeppink')
plot(penguins$flipper_length_mm[penguins$species == "Gentoo"],
     penguins$body_mass_g[penguins$species == "Gentoo"],
     xlim = xlimits,
     ylim = ylimits,
     xlab = "Flipper length (mm)",
     ylab = "Body Mass (g)",
     main = 'Gentoo',
     pch = 19, col = 'deepskyblue')


## ----multi_pdf---------------------------------------------------------------------------------
titles <- c("Adelie", "Chinstrap", "Gentoo")
colors <- c("navyblue", "deeppink", "deepskyblue")

pdf("Figures/MassvsFlipper_species.pdf", height = 4, width = 4)
for(i in 1:3){
  par(mfrow = c(1,1))
  plot(penguins$flipper_length_mm[penguins$species == titles[i]],
     penguins$body_mass_g[penguins$species == titles[i]],
     xlim = xlimits,
     ylim = ylimits,
     xlab = "Flipper length (mm)",
     ylab = "Body Mass (g)",
     main = titles[i],
     pch = 2, col = colors[i])
}
dev.off()


## ----ggplot_point, warning = F, message = F----------------------------------------------------
ggplot(penguins, aes(x = body_mass_g, y = flipper_length_mm)) +
  geom_point() +
  xlab("Body Mass (in g)") +
  ylab("Flipper length (in mm)") +
  theme_classic() +
  geom_smooth(method = lm, color = "red", se=FALSE) 
#lm = linear model
#se = whether or not to include standard error estimates


## ----warning = F, message = F------------------------------------------------------------------
ggplot(penguins, aes(x=body_mass_g, y=flipper_length_mm, color=species)) + 
    geom_point(size=6) +
    theme_minimal() +
  scale_color_manual(values = c("purple4", "darkgoldenrod", "black"),
                     ## Change legend title
                     name = "Species") +
  xlab("Body mass (in g)") + 
  ylab("Flipper length (in mm)")


## ----warning = F, message = F------------------------------------------------------------------
penguins %>% 
  ggplot(aes(x=body_mass_g, y=flipper_length_mm, color = species)) + 
  geom_point(size=2) +
  facet_wrap(~species) +
  theme_minimal() +
  guides(color = guide_legend(title = "Species")) +
  xlab("Body mass (in g)") + 
  ylab("Flipper length (in mm)")


## ----eval = FALSE, warning = F, message = F----------------------------------------------------
## penguins %>%
##   ggplot(aes(x=body_mass_g, y=flipper_length_mm)) +
##   geom_point(size=2) +
##   facet_wrap(~species) +
##   theme_classic() +
##   xlab("Body mass (in g)") +
##   ylab("Flipper length (in mm)")


## ----ggsave, warning = F, message = F----------------------------------------------------------
peng_plot <- penguins %>% 
  ggplot(aes(x=body_mass_g, y=flipper_length_mm, color = species)) + 
  geom_point(size=2) +
  facet_wrap(~species) +
  theme_minimal() +
  guides(color = guide_legend(title = "Species")) +
  xlab("Body mass (in g)") + 
  ylab("Flipper length (in mm)")

ggsave(peng_plot, filename = "Figures/MassvsFlipperLength_ggplot.pdf")

