########################################################################
#### Project: Danjugan Island Marine Reserve and Sanctuaries corals ####
#### Author: Luka Seamus Wright                                     ####
#### Date: 5th April 2021                                          ####
########################################################################

#### Univariate data ####
### Load data ####
danju <- read.csv("~/Desktop/Projects/Danjugan/DIMRS.csv")

### Data wrangling ####
## Response variables ####
HC <- danju$HC # Hard coral cover (%)
NIA <- danju$NIA # Macroalgal cover (%)
DB <- danju$DIS + danju$FBL + danju$PBL # Diseased or bleached (full + partial) coral (%)
# given as a percentage of live coral
DC <- danju$DC + danju$RKC # Dead (long + recent) coral (%)
# given as a percentage of total coral
LHC <- HC*(1-DC/100) # Live hard coral cover (%)

## Explanatory variables ####
site <- danju$site
depth <- danju$depth

### Data exploration ####
require(fitdistrplus)

## Response variables ####
par(mfrow = c(3,1), mar = c(2,2,2,1))

hist(HC)
boxplot(HC, horizontal = T)
descdist(HC, boot = 500)
# almost uniform

par(mfrow = c(3,1), mar = c(2,2,2,1))

hist(LHC)
boxplot(LHC, horizontal = T)
descdist(LHC, boot = 500)
# almost normal

hist(NIA)
boxplot(NIA, horizontal = T)
descdist(NIA, boot = 500)
# right skewed

hist(DC)
boxplot(DC, horizontal = T)
descdist(DC, boot = 500)
# close to normal

hist(DB)
boxplot(DB, horizontal = T)
descdist(DB, boot = 500)
# right skewed

par(mfrow = c(1,1), mar = c(5, 4, 4, 2) + 0.1)

## Explanatory and response variables ####
boxplot(HC ~ site) # homogenous
boxplot(LHC ~ site) # homogenous
boxplot(NIA ~ site) # heterogenous
boxplot(DC ~ site) # homogenous
boxplot(DB ~ site) # heterogenous

boxplot(HC ~ depth) 
boxplot(LHC ~ depth) 
boxplot(DC ~ depth) # no depth-effect on coral
boxplot(DB ~ depth)
boxplot(NIA ~ depth) # more algae and diseased coral in shallow water

plot(DB ~ NIA, pch = site) 
cor.test(NIA, DB) # positive correlation
plot(LHC ~ NIA, pch = site) 
cor.test(NIA, LHC) # no negative correlation

### Data analysis ####
## Total hard coral cover ####
# Build model
m1 <- lm(HC ~ site)

# Test model fit
par(mfrow = c(2,2), mar = c(2,2,2,1))
plot(m1) # quite homogenous

par(mfrow = c(1,2), mar = c(2,2,2,1))

hist(resid(m1)) # residuals are quite normally distributed
qqnorm(resid(m1))
qqline(resid(m1))

par(mfrow = c(1,1), mar = c(5, 4, 4, 2) + 0.1)
# overall m1 fits the data well

# Interpret model
require(car)
Anova(m1, type = 2) # Type II sum of squares test
# Response: HC
#           Sum Sq Df F value    Pr(>F)    
# site       10380  3  14.184 8.267e-06 ***
# Residuals   6830 28

summary(m1) # pairwise contrasts
# Bamboo vs Manta; p < 0.001 ***
# Bamboo vs Moray; p < 0.001 ***
# Bamboo vs Tabon; 0.79

site <- factor(site, levels = c("Manta", "Moray", "Tabon", "Bamboo"))
m1 <- lm(HC ~ site)
summary(m1)
# Manta vs Moray; p = 0.54
# Manta vs Tabon; p < 0.001 ***

site <- factor(site, levels = c("Tabon", "Moray", "Manta", "Bamboo"))
m1 <- lm(HC ~ site)
summary(m1)
# Tabon vs Moray; p < 0.001 ***

site <- factor(site, levels = c("Bamboo", "Manta", "Moray", "Tabon"))

## Live hard coral cover ####
# Build model
m2 <- lm(LHC ~ site)

# Test model fit
par(mfrow = c(2,2), mar = c(2,2,2,1))
plot(m2) # quite homogenous

par(mfrow = c(1,2), mar = c(2,2,2,1))

hist(resid(m2)) # residuals are normally distributed
qqnorm(resid(m2))
qqline(resid(m2))

par(mfrow = c(1,1), mar = c(5, 4, 4, 2) + 0.1)

# Interpret model
Anova(m2, type = 2) # Type II sum of squares test
# Response: LHC
#           Sum Sq Df F value    Pr(>F)    
# site      6011.9  3  11.098 5.654e-05 ***
# Residuals 5055.8 28 

summary(m2) # pairwise contrasts
# Bamboo vs Manta; p < 0.001 ***
# Bamboo vs Moray; p < 0.001 ***
# Bamboo vs Tabon; p = 0.31

site <- factor(site, levels = c("Manta", "Moray", "Tabon", "Bamboo"))
m2 <- lm(LHC ~ site)
summary(m2)
# Manta vs Moray; p = 0.25
# Manta vs Tabon; p < 0.001 ***

site <- factor(site, levels = c("Tabon", "Moray", "Manta", "Bamboo"))
m2 <- lm(LHC ~ site)
summary(m2)
# Tabon vs Moray; p = 0.009 **

site <- factor(site, levels = c("Bamboo", "Manta", "Moray", "Tabon"))

## Macroalgal cover ####
# Build model
m3 <- lm(NIA ~ site)

# Test model fit
par(mfrow = c(2,2), mar = c(2,2,2,1))
plot(m3) # heterogenous

par(mfrow = c(1,2), mar = c(2,2,2,1))

hist(resid(m3)) # normal
qqnorm(resid(m3))
qqline(resid(m3))

par(mfrow = c(1,1), mar = c(5, 4, 4, 2) + 0.1)

# try fitting gamma distribution to data
gamma <- fitdist(NIA+1, "gamma")
norm <- fitdist(NIA+1, "norm")

cdfcomp(list(gamma, norm), 
        legendtext = c("Gamma", "Normal"), 
        fitlty = 1)

gofstat(list(gamma, norm), 
        fitnames = c("Gamma", "Normal"))
# gamma fits better

# Build gamma models
m4 <- glm(NIA+1 ~ site, family = Gamma(link = "log"))

# Test model fit
par(mfrow = c(2,2), mar = c(2,2,2,1))
plot(m4) # quite homogenous

par(mfrow = c(1,2), mar = c(2,2,2,1))

hist(resid(m4)) # residuals are quite normally distributed
qqnorm(resid(m4))
qqline(resid(m4))

par(mfrow = c(1,1), mar = c(5, 4, 4, 2) + 0.1)
# m4 fits the data well

# Interpret model
Anova(m4, type = 2) # Type II sum of squares test
# Response: NIA + 1
#      LR Chisq Df Pr(>Chisq)    
# site   27.166  3  5.435e-06 ***

summary(m4) # pairwise contrasts
# Bamboo vs Manta; p = 0.01 *
# Bamboo vs Moray; p = 0.41
# Bamboo vs Tabon; p < 0.001 ***

site <- factor(site, levels = c("Manta", "Moray", "Tabon", "Bamboo"))
m4 <- glm(NIA+1 ~ site, family = Gamma(link = "log"))
summary(m4)
# Manta vs Moray; p = 0.08
# Manta vs Tabon; p = 0.05 *

site <- factor(site, levels = c("Tabon", "Moray", "Manta", "Bamboo"))
m4 <- glm(NIA+1 ~ site, family = Gamma(link = "log"))
summary(m4)
# Tabon vs Moray; p < 0.001 ***

site <- factor(site, levels = c("Bamboo", "Manta", "Moray", "Tabon"))


## Discoloured (diseased or bleached) coral ####
# Build models
m5 <- lm(DB ~ site)

# Test model fit
par(mfrow = c(2,2), mar = c(2,2,2,1))
plot(m5) # quite homogenous

par(mfrow = c(1,2), mar = c(2,2,2,1))

hist(resid(m5)) # residuals deviate at distribution edges
qqnorm(resid(m5))
qqline(resid(m5)) # but overall quite balanced

par(mfrow = c(1,1), mar = c(5, 4, 4, 2) + 0.1)

# Interpret model
Anova(m5, type = 2) # Type II sum of squares test
# Response: DB
#           Sum Sq Df F value    Pr(>F)    
# site      8517.3  3   9.297 0.0001983 ***
# Residuals 8550.6 28 

summary(m5) # pairwise contrasts
# Bamboo vs Manta; p = 0.002 **
# Bamboo vs Moray; p = 0.99
# Bamboo vs Tabon; p < 0.001 ***

site <- factor(site, levels = c("Manta", "Moray", "Tabon", "Bamboo"))
m5 <- lm(DB ~ site)
summary(m5)
# Manta vs Moray; p = 0.002 **
# Manta vs Tabon; p = 0.5

site <- factor(site, levels = c("Tabon", "Moray", "Manta", "Bamboo"))
m5 <- lm(DB ~ site)
summary(m5)
# Tabon vs Moray; p < 0.001 ***

site <- factor(site, levels = c("Bamboo", "Manta", "Moray", "Tabon"))


## Dead coral (% of total coral) ####
# Build model
m6 <- lm(DC ~ site)

# Test model fit
par(mfrow = c(2,2), mar = c(2,2,2,1))
plot(m6) # quite homogenous

par(mfrow = c(1,2), mar = c(2,2,2,1))

hist(resid(m6)) # normality is ok
qqnorm(resid(m6))
qqline(resid(m6))

par(mfrow = c(1,1), mar = c(5, 4, 4, 2) + 0.1)

# Interpret model
Anova(m6, type = 2) # Type II sum of squares test
# Response: DC
#           Sum Sq Df F value    Pr(>F)    
# site      7445.2  3  8.0249 0.0005162 ***
# Residuals 8659.1 28 

summary(m6) # pairwise contrasts
# Bamboo vs Manta; p < 0.001 ***
# Bamboo vs Moray; p = 0.04 *
# Bamboo vs Tabon; p < 0.001 ***

site <- factor(site, levels = c("Manta", "Moray", "Tabon", "Bamboo"))
m6 <- lm(DC ~ site)
summary(m6)
# Manta vs Moray; p = 0.04 *
# Manta vs Tabon; p = 0.93

site <- factor(site, levels = c("Tabon", "Moray", "Manta", "Bamboo"))
m6 <- lm(DC ~ site)
summary(m6)
# Tabon vs Moray; p = 0.05

site <- factor(site, levels = c("Manta", "Moray", "Tabon", "Bamboo"))


### Data visualisation ####
## Calculate descriptive statistics ####
require(psych)
HCstat <- describeBy(HC, site, mat = T)
NIAstat <- describeBy(NIA, site, mat = T)
DBstat <- describeBy(DB, site, mat = T)

## Modify coral cover dataframe ####
HCstat2 <- rbind(describeBy(LHC, site, mat = T), # live coral cover
                 describeBy(HC*DC/100, site, mat = T)) # dead coral cover
HCstat2$group2 <- c(rep("live", 4), rep("dead", 4))
HCstat2$mean2 <- rep(HCstat$mean, 2)
HCstat2$se2 <- rep(HCstat$se, 2)

## Load historical data and combine ####
# this data was sourced from https://wedocs.unep.org/handle/20.500.11822/29101
# hdf <- data.frame(year = c(rep(2002+8/12, 4), rep(2002+11/12, 4),
#                            rep(2003+2/12, 4), rep(2003+5/12, 4),
#                            rep(2016+12/12, 4)),
#                   site = rep(c("Manta", "Moray", "Tabon", "Bamboo"), 5),
#                   LC = c(48, 43, 42, NA, 42, 59, 37, NA, 54, 48, 46, NA, 47, 52, 41, NA,
#                          HCstat2$mean[1:4]),
#                   DC = c(13, 23, 23, NA, 6, 25, 8, NA, 6, 16, 7, NA, 2, 23, 3, NA,
#                          HCstat2$mean[5:8]),
#                   HC = c(61, 66, 65, NA, 48, 84, 45, NA, 60, 64, 53, NA, 49, 75, 44, NA,
#                          HCstat2$mean2[1:4]),
#                   NIA = c(16, 9, 14, NA, 23, 5, 13, NA, 19, 12, 31, NA, 34, 7, 31, NA,
#                           NIAstat$mean),
#                   LCse = c(rep(0, 16), HCstat2$se[1:4]),
#                   DCse = c(rep(0, 16), HCstat2$se[5:8]),
#                   HCse = c(rep(0, 16), HCstat2$se2[1:4]),
#                   NIAse = c(rep(0, 16), NIAstat$se))

hdf <- data.frame(year = c(rep(2002, 4), rep(2003, 4), rep(2016, 4)),
                  site = rep(c("Manta", "Moray", "Tabon", "Bamboo"), 3),
                  LC = c(45, 51, 39.5, NA, 50.5, 50, 43.5, NA, HCstat2$mean[1:4]),
                  DC = c(9.5, 24, 15.5, NA, 4, 19.5, 5, NA, HCstat2$mean[5:8]),
                  HC = c(54.5, 75, 55, NA, 54.5, 69.5, 48.5, NA, HCstat2$mean2[1:4]),
                  NIA = c(19.5, 7, 13.5, NA, 26.5, 9.5, 31, NA, NIAstat$mean),
                  LCse = c(rep(0, 8), HCstat2$se[1:4]),
                  DCse = c(rep(0, 8), HCstat2$se[5:8]),
                  HCse = c(rep(0, 8), HCstat2$se2[1:4]),
                  NIAse = c(rep(0, 8), NIAstat$se))

# note that this study used a different control site for the east side of the island
# hence, Bamboo is not represented in the data



## Set theme ####
require(ggplot2)
mytheme <- theme(panel.background = element_blank(),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.border = element_blank(),
                 axis.line = element_line(),
                 axis.title = element_text(size = 15),
                 axis.title.x = element_blank(),
                 axis.text = element_text(size = 12, colour = "black"),
                 axis.ticks.length = unit(.25, "cm"),
                 axis.ticks = element_line(colour = "black"),
                 axis.ticks.x = element_blank(),
                 legend.key = element_blank(),
                 legend.background = element_blank(),
                 legend.text = element_text(size = 12),
                 legend.text.align = 0,
                 legend.title = element_blank(),
                 text = element_text(family = "Helvetica Neue"))

## Plot ####
# Total hard coral cover (simple)
HCplot <- ggplot(HCstat, mapping = aes(factor(group1, levels = unique(group1)), mean)) +
              geom_col(width = 0.7, fill = c("#7fb8d5", "#467289", "#f7b060", "#ef8407")) +
              geom_errorbar(mapping = aes(ymin = mean - se, ymax = mean + se),
                            width = .1, lwd = .4) +
              geom_text(aes(x = group1, y = mean + se + 2,
                            label = c("a","a","b","b"))) +
              ylab("Scleractinian coral cover (%)") +
              scale_y_continuous(expand = c(0,0)) +
              coord_cartesian(ylim = c(0, 80)) +
              mytheme
HCplot # dimensions: 4 x 4 in

# Coral cover (including split between live and dead)
HCplot2 <- ggplot(HCstat2, mapping = aes(factor(group1, levels = unique(group1)),
                                         mean, alpha = group2)) +
              geom_col(position = "stack", width = 0.7,
                       fill = rep(c("#7fb8d5", "#467289", "#f7b060", "#ef8407"), 2)) +
              geom_errorbar(mapping = aes(ymin = mean2 - se2, ymax = mean2 + se2),
                            width = .1, lwd = .4) +
              geom_text(aes(x = group1, y = mean2 + se2 + 2,
                            label = rep(c("a","a","b","b"), 2))) +
              scale_alpha_manual(values = c(0.5, 1),
                                 labels = c("Dead", "Live"),
                                 guide = "none") +
              ylab("Scleractinian coral cover (%)") +
              scale_y_continuous(expand = c(0,0)) +
              coord_cartesian(ylim = c(0, 80)) +
              mytheme
HCplot2 # dimensions: 4 x 4 in

# Coral cover (including historical data)
hdf$site <- factor(hdf$site, levels = unique(hdf$site))
HCplot3 <- ggplot(hdf, mapping = aes(year, HC, colour = site)) +
              geom_line(aes(group = site), size = 0.6, position = position_dodge(width = -3.2)) +
              geom_pointrange(aes(ymin = HC - HCse, ymax = HC + HCse,
                                  shape = site, fill = site), size = 0.6,
                              position = position_dodge(width = -3.2)) +
              annotate("text", x = c(2018.4, 2017.1), y = c(57.01219, 21.18903),
                       label = c("a", "b")) +
              scale_colour_manual(values = c("#7fb8d5", "#467289", "#f7b060", "#ef8407"),
                                  labels = c("Manta", "Moray", "Tabon", "Bamboo"),
                                  guide = guide_legend()) +
              scale_fill_manual(values = c("#7fb8d5", "#467289", "#f7b060", "#ef8407"),
                                labels = c("Manta", "Moray", "Tabon", "Bamboo"),
                                guide = guide_legend()) +
              scale_shape_manual(values = 21:24,
                                 labels = c("Manta", "Moray", "Tabon", "Bamboo"),
                                 guide = guide_legend()) +
              ylab("Scleractinian coral cover (%)") +
              scale_y_continuous(expand = c(0,0)) +
              scale_x_continuous(expand = c(0,0), breaks = seq(2000, 2018, by = 9)) +
              coord_cartesian(ylim = c(0, 100), xlim = c(2000, 2018),
                              clip = "off") +
              mytheme +
              theme(legend.position = c(0.88, 0.88),
                    axis.ticks.x = element_line(colour = "black"),
                    plot.margin = unit(c(0.2, 0.5, 0.2, 0.2),"cm"))

HCplot3 # dimensions: 4 x 4 in

# Live coral cover (including historical data)
LCplot <- ggplot(hdf, mapping = aes(year, LC, colour = site)) +
              geom_pointrange(aes(ymin = LC - LCse, ymax = LC + LCse,
                                  shape = site, fill = site), size = 0.6,
                              position = position_dodge(width = -3.8)) +
              geom_line(aes(group = site), size = 0.6, position = position_dodge(width = -3.8)) +
              annotate("text", x = c(2018.3, 2016.6), y = c(39.32926, 12.95731),
                       label = c("a", "b")) +
              scale_colour_manual(values = c("#7fb8d5", "#467289", "#f7b060", "#ef8407"),
                                  labels = c("Manta", "Moray", "Tabon", "Bamboo"),
                                  guide = guide_legend()) +
              scale_fill_manual(values = c("#7fb8d5", "#467289", "#f7b060", "#ef8407"),
                                labels = c("Manta", "Moray", "Tabon", "Bamboo"),
                                guide = guide_legend()) +
              scale_shape_manual(values = 21:24,
                                 labels = c("Manta", "Moray", "Tabon", "Bamboo"),
                                 guide = guide_legend()) +
              ylab("Live scleractinian coral cover (%)") +
              scale_y_continuous(expand = c(0,0)) +
              scale_x_continuous(expand = c(0,0), breaks = seq(2000, 2018, by = 9)) +
              coord_cartesian(ylim = c(0, 100), xlim = c(2000, 2018),
                              clip = "off") +
              mytheme +
              theme(legend.position = c(0.88, 0.88),
                    axis.ticks.x = element_line(colour = "black"),
                    plot.margin = unit(c(0.2, 0.5, 0.2, 0.2),"cm"))

LCplot # dimensions: 4 x 4 in

# Macroalgal cover
NIAplot <- ggplot(NIAstat, mapping = aes(factor(group1, levels = unique(group1)), mean)) +
              geom_col(width = 0.7, fill = c("#7fb8d5", "#467289", "#f7b060", "#ef8407")) +
              geom_errorbar(mapping = aes(ymin = mean - se, ymax = mean + se),
                            width = .1, lwd = .4) +
              geom_text(aes(x = group1, y = mean + se + 1.3,
                            label = c("a","ab","c","b"))) +
              ylab("Macroalgal cover (%)") +
              scale_y_continuous(expand = c(0,0)) +
              coord_cartesian(ylim = c(0, 50)) +
              mytheme
NIAplot # dimensions: 4 x 4 in

# Macroalgal cover (including historical data)
NIAplot2 <- ggplot(hdf, mapping = aes(year, NIA, colour = site)) +
              geom_pointrange(aes(ymin = NIA - NIAse, ymax = NIA + NIAse,
                                  shape = site, fill = site), size = 0.6,
                              position = position_dodge(width = -1)) +
              geom_line(aes(group = site), size = 0.6, position = position_dodge(width = -1)) +
              annotate("text", x = c(2017.6, 2017.6, 2017.2, 2017), y = hdf$NIA[9:12],
                       label = c("b", "bc", "a", "c")) +
              scale_colour_manual(values = c("#7fb8d5", "#467289", "#f7b060", "#ef8407"),
                                  labels = c("Manta", "Moray", "Tabon", "Bamboo"),
                                  guide = guide_legend()) +
              scale_fill_manual(values = c("#7fb8d5", "#467289", "#f7b060", "#ef8407"),
                                labels = c("Manta", "Moray", "Tabon", "Bamboo"),
                                guide = guide_legend()) +
              scale_shape_manual(values = 21:24,
                                 labels = c("Manta", "Moray", "Tabon", "Bamboo"),
                                 guide = guide_legend()) +
              ylab("Macroalgal cover (%)") +
              scale_y_continuous(expand = c(0,0)) +
              scale_x_continuous(expand = c(0,0), breaks = seq(2000, 2018, by = 9)) +
              coord_cartesian(ylim = c(0, 80), xlim = c(2000, 2018),
                              clip = "off") +
              mytheme +
              theme(legend.position = c(0.88, 0.88),
                    axis.ticks.x = element_line(colour = "black"),
                    plot.margin = unit(c(0.2, 0.5, 0.2, 0.2),"cm"))

NIAplot2 # dimensions: 4 x 4 in

# Discoloured coral
DBplot <- ggplot(DBstat, mapping = aes(factor(group1, levels = unique(group1)), mean)) +
              geom_col(width = 0.7, fill = c("#7fb8d5", "#467289", "#f7b060", "#ef8407")) +
              geom_errorbar(mapping = aes(ymin = mean - se, ymax = mean + se),
                            width = .1, lwd = .4) +
              geom_text(aes(x = group1, y = mean + se + 1.5,
                            label = c("a","b","a","b"))) +
              ylab("Discoloured scleractinian coral (%)") +
              scale_y_continuous(expand = c(0,0)) +
              coord_cartesian(ylim = c(0, 60)) +
              mytheme
DBplot # dimensions: 4 x 4 in

# "Morphospecies" richness (point range)
DBplot2 <- ggplot(DBstat, mapping = aes(factor(group1, levels = unique(group1)), mean)) +
              geom_pointrange(aes(ymin = mean - se, ymax = mean + se,
                                  colour = factor(group1, levels = unique(group1)),
                                  fill = factor(group1, levels = unique(group1)),
                                  shape = factor(group1, levels = unique(group1))),
                              size = 0.6) +
              scale_colour_manual(values = c("#7fb8d5", "#467289", "#f7b060", "#ef8407"),
                                  labels = c("Manta", "Moray", "Tabon", "Bamboo"),
                                  guide = guide_legend()) +
              scale_fill_manual(values = c("#7fb8d5", "#467289", "#f7b060", "#ef8407"),
                                labels = c("Manta", "Moray", "Tabon", "Bamboo"),
                                guide = guide_legend()) +
              scale_shape_manual(values = 21:24,
                                 labels = c("Manta", "Moray", "Tabon", "Bamboo"),
                                 guide = guide_legend()) +
              geom_text(aes(x = group1, y = mean + se + 3,
                            label = c("a","b","a","b"))) +
              scale_y_continuous(expand = c(0,0)) +
              ylab("Discoloured scleractinian coral (%)") +
              coord_cartesian(ylim = c(0, 80)) +
              mytheme +
              theme(legend.position = "none",
                    axis.text.x = element_blank(),
                    axis.line.x = element_blank())
DBplot2 # dimensions: 4 x 4 in


#### Multivariate "morphospecies" data ####
require(vegan)

### Load data ####
morph <- danju[,c(1:6, 23:32)]
str(morph)

### Data wrangling ####
## Reorder levels of site factor ####
morph$site <- factor(morph$site, levels = c("Manta", "Moray", "Tabon", "Bamboo"))
str(morph)

## Define environmental variables ####
env <- morph[,1:5]

## Define "morphospecies" abundance data ####
coral <- morph[,6:16]
coral <- data.frame(coral, row.names = 1)

### Data analysis ####
## Calculate Bray-Curtis dissimilarity ####
coral.dist <- vegdist(coral, method = "bray")

## Test for homogeneity of dispersion ####
betad <- with(env, betadisper(coral.dist, site))
permutest(betad, permutations = 9999) # heterogenous

## Transform ####
sqrt.coral <- sqrt(coral)

## Calculate Bray-Curtis dissimilarity ####
coral.dist <- vegdist(sqrt.coral, method = "bray")

## Test for homogeneity of dispersion ####
betad <- with(env, betadisper(coral.dist, site))
permutest(betad, permutations = 9999) # homogenous

## PERMANOVA, "morphospecies" abundance data ####
adonis(sqrt.coral ~ site, env, permutations = 9999)
#           Df SumsOfSqs MeanSqs F.Model      R2 Pr(>F)    
# site       3    1.4676 0.48919  5.4988 0.37074  1e-04 ***
# Residuals 28    2.4909 0.08896         0.62926           
# Total     31    3.9585                 1.00000 

# site significantly affects the community composition of 
# "morphospecies" and explains ~37% of the variance in 
# community composition

## SIMPER, "morphospecies" influence on dissimilarity between sites ####
morphospecies <- with(env, simper(sqrt.coral, site, permutations = 9999))
summary(morphospecies, ordered = T)
# corymbose, solitary and submassive coral seem to be a major drivers of dissimilarity
# -> worth looking into some univariate analysis

## Data exploration ####
boxplot(danju$S ~ site)
boxplot(danju$SM ~ site) # looks interesting
boxplot(danju$C ~ site) # looks interesting

site <- factor(site, levels = c("Bamboo", "Manta", "Moray", "Tabon"))

## Data analysis ####
## Corymbose coral (% of total coral) ####
# Build model
C <- danju$C
m7 <- lm(C ~ site)

# Test model fit
par(mfrow = c(2,2), mar = c(2,2,2,1))
plot(m7) # quite homogenous

par(mfrow = c(1,2), mar = c(2,2,2,1))

hist(resid(m7)) # residuals are normally distributed
qqnorm(resid(m7))
qqline(resid(m7))

par(mfrow = c(1,1), mar = c(5, 4, 4, 2) + 0.1)

# Interpret model
Anova(m7, type = 2) # Type II sum of squares test
# Response: C
#            Sum Sq Df F value    Pr(>F)    
# site      12095.9  3  16.597 2.164e-06 ***
# Residuals  6802.2 28  

summary(m7)
# Bamboo vs Manta; p = 0.42
# Bamboo vs. Moray; p < 0.001 ***
# Bamboo vs. Tabon; p = 0.07

site <- factor(site, levels = c("Manta", "Moray", "Tabon", "Bamboo"))
m7 <- lm(C ~ site)
summary(m7)
# Manta vs Moray; p < 0.001 ***
# Manta vs Tabon; p = 0.01 *

site <- factor(site, levels = c("Tabon", "Moray", "Manta", "Bamboo"))
m7 <- lm(C ~ site)
summary(m7)
# Tabon vs Moray; p < 0.001 ***

site <- factor(site, levels = c("Bamboo", "Manta", "Moray", "Tabon"))

## Submassive coral (% of total coral) ####
# Build models
SM <- danju$SM
m8 <- lm(SM ~ site)

# Test model fit
par(mfrow = c(2,2), mar = c(2,2,2,1))
plot(m8) # quite homogenous

par(mfrow = c(1,2), mar = c(2,2,2,1))

hist(resid(m8)) # residuals are quite normally distributed
qqnorm(resid(m8))
qqline(resid(m8))

par(mfrow = c(1,1), mar = c(5, 4, 4, 2) + 0.1)

# Interpret model
Anova(m8, type = 2) # Type II sum of squares test
# Response: SM
#           Sum Sq Df F value    Pr(>F)    
# site       11413  3  8.8594 0.0002736 ***
# Residuals  12023 28 

summary(m8) # pairwise contrasts
# Bamboo vs Manta; p = 0.11
# Bamboo vs. Moray; p = 0.46
# Bamboo vs. Tabon; p < 0.001 ***

site <- factor(site, levels = c("Manta", "Moray", "Tabon", "Bamboo"))
m8 <- lm(SM ~ site)
summary(m8)
# Manta vs Moray; p = 0.02 *
# Manta vs Tabon; p = 0.02 *

site <- factor(site, levels = c("Tabon", "Moray", "Manta", "Bamboo"))
m8 <- lm(SM ~ site)
summary(m8)
# Tabon vs Moray; p < 0.001 ***

site <- factor(site, levels = c("Bamboo", "Manta", "Moray", "Tabon"))

## Calculate diversity indices ####
S <- specnumber(coral) # Species richness
D <- diversity(coral, index = "invsimpson", base = exp(1)) # Simpson's reciprocal index (1/D)
D2 <- diversity(coral, index = "simpson", base = exp(1)) # Simpson's diversity index (1-D)
H <- diversity(coral, index = "shannon", base = exp(1)) # Shannon-Wiener diversity index
E <- D/S # Simpson's evenness index
J <- H/log(S) # Pielou's evenness index
J # NaN produced -> proceed with E instead of J

## "Morphospecies" richness ####
# Build model
m9 <- lm(S ~ site)

# Test model fit
par(mfrow = c(2,2), mar = c(2,2,2,1))
plot(m9) # quite homogenous

par(mfrow = c(1,2), mar = c(2,2,2,1))

hist(resid(m9)) # residuals are quite normally distributed
qqnorm(resid(m9))
qqline(resid(m9))

par(mfrow = c(1,1), mar = c(5, 4, 4, 2) + 0.1)

# Interpret model
Anova(m9, type = 2) # Type II sum of squares test
# Response: S
#           Sum Sq Df F value   Pr(>F)   
# site      23.125  3  5.2965 0.005094 **
# Residuals 40.750 28     

summary(m9) # pairwise contrasts
# Bamboo vs Manta; p = 0.11
# Bamboo vs. Moray; p = 0.84
# Bamboo vs. Tabon; p = 0.03 *

site <- factor(site, levels = c("Manta", "Moray", "Tabon", "Bamboo"))
m9 <- lm(S ~ site)
summary(m9)
# Manta vs Moray; p = 0.16
# Manta vs Tabon; p < 0.001 ***

site <- factor(site, levels = c("Tabon", "Moray", "Manta", "Bamboo"))
m9 <- lm(S ~ site)
summary(m9)
# Tabon vs Moray; p = 0.02 *

site <- factor(site, levels = c("Bamboo", "Manta", "Moray", "Tabon"))


## Simpson's evenness ####
# Build model
m10 <- lm(E ~ site)

# Test model fit
par(mfrow = c(2,2), mar = c(2,2,2,1))
plot(m10) # quite homogenous

par(mfrow = c(1,2), mar = c(2,2,2,1))

hist(resid(m10)) # residuals are quite normally distributed
qqnorm(resid(m10))
qqline(resid(m10))

par(mfrow = c(1,1), mar = c(5, 4, 4, 2) + 0.1)

# Interpret model
Anova(m10, type = 2) # Type II sum of squares test
# Response: E
#           Sum Sq Df F value    Pr(>F)    
# site      0.95375  3  22.052 1.558e-07 ***
# Residuals 0.40368 28     

summary(m10) # pairwise contrasts
# Bamboo vs Manta; p < 0.001 ***
# Bamboo vs. Moray; p < 0.001 ***
# Bamboo vs. Tabon; p = 0.81

site <- factor(site, levels = c("Manta", "Moray", "Tabon", "Bamboo"))
m10 <- lm(E ~ site)
summary(m10)
# Manta vs Moray; p = 0.94
# Manta vs Tabon; p < 0.001 ***

site <- factor(site, levels = c("Tabon", "Moray", "Manta", "Bamboo"))
m10 <- lm(E ~ site)
summary(m10)
# Tabon vs Moray; p < 0.001 ***

site <- factor(site, levels = c("Bamboo", "Manta", "Moray", "Tabon"))


### Data visualisation ####
## Calculate nMDS data ####
coralmds <- metaMDS(sqrt.coral, distance = "bray", autotransform = FALSE)
coralmds$stress # stress = 0.18
# NB metaMDS function may have to be run several times before convergence 
# (i.e. multidimentional space arrangement solution) is reached. 
# nMDS without convergence is not reliable and will result in a markedly 
# different plot and interpretation of results.

## Calculate data points and 95% confidence ellipses ####
site <- factor(site, levels = c("Manta", "Moray", "Tabon", "Bamboo"))

df <- data.frame(coralmds$points, group = site)
plot(coralmds)
ellipse <- ordiellipse(coralmds, site, display = "sites", kind = "se",
                       conf = 0.95, label = F)

## Make data compatible with ggplot2
# the following function was sourced from 
# https://github.com/jarioksa/vegan/blob/master/R/veganCovEllipse.R
veganCovEllipse <- function (cov, center = c(0, 0), scale = 1, npoints = 100) 
{
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  Q <- chol(cov, pivot = TRUE)
  o <- attr(Q, "pivot")
  t(center + scale * t(Circle %*% Q[,o]))
}

dfell <- data.frame()

for(g in levels(df$group)) {
  dfell <- rbind(dfell, 
                 cbind(as.data.frame(with(df[df$group==g,],
                                                 veganCovEllipse(ellipse[[g]]$cov,
                                                                 ellipse[[g]]$center,
                                                                 ellipse[[g]]$scale))),
                              group = g))
}

## Define colours, shapes and factor level order ####
colours <- c("#7fb8d5", "#467289", "#f7b060", "#ef8407")
shapes <- c(21:24)

## Plot ####
morph.plot <- ggplot(data = df, aes(MDS1, MDS2)) +
                  geom_point(aes(colour = group, shape = group, fill = group),
                             size = 3) +
                  geom_polygon(data = dfell, aes(NMDS1, NMDS2, colour = group,
                                   fill = group), size = 0.7, alpha = 0.3) +
                  scale_fill_manual(values = colours,
                                    labels = c("Manta", "Moray", "Tabon", "Bamboo"),
                                    guide = guide_legend(title = "Site")) +
                  annotate("text", label = "Stress = 0.18", x = -0.74, y = 0.54,
                           size = 4.2) +
                  scale_colour_manual(values = colours,
                                      labels = c("Manta", "Moray", "Tabon", "Bamboo"),
                                      guide = guide_legend(title = "Site")) +
                  scale_shape_manual(values = shapes,
                                     labels = c("Manta", "Moray", "Tabon", "Bamboo"),
                                     guide = guide_legend(title = "Site")) +
                  mytheme +
                  theme(legend.position = c(0.15, 0.86),
                        axis.title = element_blank(),
                        axis.ticks = element_blank(),
                        axis.text = element_blank(),
                        axis.line = element_blank(),
                        panel.border = element_rect(fill = NA, size = 1)) +
                  coord_cartesian(y = c(-0.8, 1.1))

morph.plot # dimensions: 4 x 4 in


## Calculate univariate descriptive statistics ####
Cstat <- describeBy(C, site, mat = T)
SMstat <- describeBy(SM, site, mat = T)
Sstat <- describeBy(S, site, mat = T)
Estat <- describeBy(E, site, mat = T)

## Plot ####
# Corymbose coral
Cplot <- ggplot(Cstat, mapping = aes(factor(group1, levels = unique(group1)), mean)) +
              geom_col(width = 0.7, fill = c("#7fb8d5", "#467289", "#f7b060", "#ef8407")) +
              geom_errorbar(mapping = aes(ymin = mean - se, ymax = mean + se),
                            width = .1, lwd = .4) +
              geom_text(aes(x = group1, y = mean + se + 2,
                            label = c("a","b","c","ac"))) +
              ylab("Corymbose scleractinian coral (%)") +
              scale_y_continuous(expand = c(0,0)) +
              coord_cartesian(ylim = c(0, 80)) +
              mytheme
Cplot # dimensions: 4 x 4 in

# Submassive coral
SMplot <- ggplot(SMstat, mapping = aes(factor(group1, levels = unique(group1)), mean)) +
              geom_col(width = 0.7, fill = c("#7fb8d5", "#467289", "#f7b060", "#ef8407")) +
              geom_errorbar(mapping = aes(ymin = mean - se, ymax = mean + se),
                            width = .1, lwd = .4) +
              geom_text(aes(x = group1, y = mean + se + 2,
                            label = c("a","b","c","ab"))) +
              ylab("Submassive scleractinian coral (%)") +
              scale_y_continuous(expand = c(0,0)) +
              coord_cartesian(ylim = c(0, 80)) +
              mytheme
SMplot # dimensions: 4 x 4 in

# "Morphospecies" richness
Splot <- ggplot(Sstat, mapping = aes(factor(group1, levels = unique(group1)), mean)) +
              geom_col(width = 0.7, fill = c("#7fb8d5", "#467289", "#f7b060", "#ef8407")) +
              geom_errorbar(mapping = aes(ymin = mean - se, ymax = mean + se),
                            width = .1, lwd = .4) +
              geom_text(aes(x = group1, y = mean + se + 0.16,
                            label = c("a","a","b","a"))) +
              scale_y_continuous(expand = c(0,0)) +
              ylab("Number of growth morphologies") +
              coord_cartesian(ylim = c(0, 6)) +
              mytheme
Splot # dimensions: 4 x 4 in

# "Morphospecies" richness (point range)
Splot2 <- ggplot(Sstat, mapping = aes(factor(group1, levels = unique(group1)), mean)) +
              geom_pointrange(aes(ymin = mean - se, ymax = mean + se,
                                  colour = factor(group1, levels = unique(group1)),
                                  fill = factor(group1, levels = unique(group1)),
                                  shape = factor(group1, levels = unique(group1))),
                              size = 0.6) +
              scale_colour_manual(values = c("#7fb8d5", "#467289", "#f7b060", "#ef8407"),
                                  labels = c("Manta", "Moray", "Tabon", "Bamboo"),
                                  guide = guide_legend()) +
              scale_fill_manual(values = c("#7fb8d5", "#467289", "#f7b060", "#ef8407"),
                                labels = c("Manta", "Moray", "Tabon", "Bamboo"),
                                guide = guide_legend()) +
              scale_shape_manual(values = 21:24,
                                 labels = c("Manta", "Moray", "Tabon", "Bamboo"),
                                 guide = guide_legend()) +
              geom_text(aes(x = group1, y = mean + se + 0.3,
                            label = c("a","a","b","a"))) +
              scale_y_continuous(expand = c(0,0)) +
              ylab("Number of growth morphologies") +
              coord_cartesian(ylim = c(0, 8)) +
              mytheme +
              theme(legend.position = c(0.88, 0.88),
                    axis.text.x = element_blank(),
                    axis.line.x = element_blank())
Splot2 # dimensions: 4 x 4 in

# Simpson's evenness
Eplot <- ggplot(Estat, mapping = aes(factor(group1, levels = unique(group1)), mean)) +
              geom_col(width = 0.7, fill = c("#7fb8d5", "#467289", "#f7b060", "#ef8407")) +
              geom_errorbar(mapping = aes(ymin = mean - se, ymax = mean + se),
                            width = .1, lwd = .4) +
              geom_text(aes(x = group1, y = mean + se + 0.028,
                            label = c("a","a","b","b"))) +
              ylab("Simpson's evenness index") +
              scale_y_continuous(expand = c(0,0)) +
              coord_cartesian(ylim = c(0, 1)) +
              mytheme
Eplot # dimensions: 4 x 4 in

# Simpson's evenness (point range)
Eplot2 <- ggplot(Estat, mapping = aes(factor(group1, levels = unique(group1)), mean)) +
              geom_pointrange(aes(ymin = mean - se, ymax = mean + se,
                                  colour = factor(group1, levels = unique(group1)),
                                  fill = factor(group1, levels = unique(group1)),
                                  shape = factor(group1, levels = unique(group1))),
                              size = 0.6) +
              scale_colour_manual(values = c("#7fb8d5", "#467289", "#f7b060", "#ef8407"),
                                  labels = c("Manta", "Moray", "Tabon", "Bamboo"),
                                  guide = guide_legend()) +
              scale_fill_manual(values = c("#7fb8d5", "#467289", "#f7b060", "#ef8407"),
                                labels = c("Manta", "Moray", "Tabon", "Bamboo"),
                                guide = guide_legend()) +
              scale_shape_manual(values = 21:24,
                                 labels = c("Manta", "Moray", "Tabon", "Bamboo"),
                                 guide = guide_legend()) +
              geom_text(aes(x = group1, y = mean + se + 0.04,
                            label = c("a","a","b","b"))) +
              scale_y_continuous(expand = c(0,0)) +
              ylab("Simpson's evenness index") +
              coord_cartesian(ylim = c(0, 1)) +
              mytheme +
              theme(legend.position = "none",
                    axis.text.x = element_blank(),
                    axis.line.x = element_blank())
Eplot2 # dimensions: 4 x 4 in

#### Multivariate substratum data ####
### Load data ####
sub <- danju[,1:16]
str(sub)

### Data wrangling ####
## Reorder levels of site factor ####
sub$site <- factor(sub$site, levels = c("Manta", "Moray", "Tabon", "Bamboo"))
str(sub)

## Define environmental variables ####
env <- sub[,1:5]

## Define species abundance data ####
subtyp <- sub[,6:16]
subtyp <- data.frame(subtyp, row.names = 1)

### Data analysis ####
## Calculate Bray-Curtis dissimilarity ####
sub.dist <- vegdist(subtyp, method = "bray")

## Test for homogeneity of dispersion ####
betad <- with(env, betadisper(sub.dist, site))
permutest(betad, permutations = 9999) # homogenous

## PERMANOVA, substratum quantity data ####
adonis(subtyp ~ site, env, permutations = 9999)
#           Df SumsOfSqs MeanSqs F.Model      R2 Pr(>F)    
# site       3    2.6534 0.88446  13.587 0.59279  1e-04 ***
# Residuals 28    1.8227 0.06510         0.40721           
# Total     31    4.4761                 1.00000 

# site significantly affects the substratum composition
# and explains ~59% of the variance in substratum composition

## SIMPER, substratum influence on dissimilarity between sites ####
substrata <- with(env, simper(subtyp, site, permutations = 9999))
summary(substrata, ordered = T)
# scleratinian coral seems to be the main driver of dissimilarity,
# especially between protected and unprotected sites

### Data visualisation ####
## Calculate nMDS data ####
submds <- metaMDS(subtyp, autotransform = FALSE)
submds$stress # stress = 0.06

## Calculate data points and 95% confidence ellipses ####
site <- factor(site, levels = c("Manta", "Moray", "Tabon", "Bamboo"))

df <- data.frame(submds$points, group = site)
ellipse <- ordiellipse(submds, site, display = "sites", kind = "se",
                       conf = 0.95, label = F)

## Make data compatible with ggplot2 ####
dfell <- data.frame()

for(g in levels(df$group)) {
  dfell <- rbind(dfell, 
                 cbind(as.data.frame(with(df[df$group==g,],
                                          veganCovEllipse(ellipse[[g]]$cov,
                                                          ellipse[[g]]$center,
                                                          ellipse[[g]]$scale))),
                       group = g))
}


## Plot ####
sub.plot <- ggplot(data = df, aes(MDS1, MDS2)) +
                  geom_point(aes(colour = group, shape = group, fill = group),
                             size = 3) +
                  geom_polygon(data = dfell, aes(NMDS1, NMDS2, colour = group,
                                   fill = group), size = 0.7, alpha = 0.3) +
                  scale_fill_manual(values = colours,
                                    labels = c("Manta", "Moray", "Tabon", "Bamboo"),
                                    guide = guide_legend(title = "Site")) +
                  annotate("text", label = "Stress = 0.06", x = 0.59, y = -0.859,
                           size = 4.2) +
                  scale_colour_manual(values = colours,
                                      labels = c("Manta", "Moray", "Tabon", "Bamboo"),
                                      guide = guide_legend(title = "Site")) +
                  scale_shape_manual(values = shapes,
                                     labels = c("Manta", "Moray", "Tabon", "Bamboo"),
                                     guide = guide_legend(title = "Site")) +
                  mytheme +
                  coord_flip() +
                  theme(legend.position = c(0.15, 0.86),
                        axis.title = element_blank(),
                        axis.ticks = element_blank(),
                        axis.text = element_blank(),
                        axis.line = element_blank(),
                        panel.border = element_rect(fill = NA, size = 1))

sub.plot # dimensions: 4 x 4 in

### Combined plots ####
require(cowplot)
sub.plot <- sub.plot + theme(plot.margin = unit(c(0, 0, 0.56, 0.56),"cm"),
                             legend.position = c(0.132, 0.86))
HCplot2 <- HCplot2 + theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.6),"cm"))
Fig2 <- plot_grid(sub.plot, HCplot2, labels = "auto", label_size = 15)
Fig2 # 4 x 8 in

HCplot3 <- HCplot3 + theme(legend.position = "none", 
                           plot.margin = unit(c(0.2, 0.5, 0.2, 0.5),"cm"))
LCplot <- LCplot + theme(legend.position = c(.72, .88),
                         axis.text.y = element_blank(), 
                         axis.title.y = element_blank(),
                         plot.margin = unit(c(0.2, 0.5, 0.2, 0.8),"cm"))
Fig2v2 <- plot_grid(sub.plot, HCplot3, LCplot, nrow = 1, 
                    rel_widths = c(1, 0.672, 0.578), labels = "auto", label_size = 15)
Fig2v2 # 4 x 9 in

morph.plot <- morph.plot + theme(plot.margin = unit(c(0, 0, 0.56, 0.56),"cm"),
                                 legend.position = c(0.132, 0.86))
Eplot <- Eplot + theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.4),"cm"))
Fig3 <- plot_grid(morph.plot, Eplot, labels = "auto", label_size = 15)
Fig3 # 4 x 8 in

Splot2 <- Splot2 + theme(plot.margin = unit(c(0.2, 0.2, 0.58, 0.56),"cm"),
                         legend.position = c(.72, .88))
Eplot2 <- Eplot2 + theme(plot.margin = unit(c(0.2, 0.2, 0.58, 0.56),"cm"))
Fig3v2 <- plot_grid(morph.plot, Splot2, Eplot2, nrow = 1,
                    rel_widths = c(1, 0.595, 0.655), labels = "auto", label_size = 15)
Fig3v2 # 4 x 9 in

NIAplot <- NIAplot + theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.6),"cm"))
DBplot <- DBplot + theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.6),"cm"))
FigS1 <- plot_grid(NIAplot, DBplot, labels = "auto", label_size = 15)
FigS1 # 4 x 8 in

NIAplot2 <- NIAplot2 + theme(plot.margin = unit(c(0.2, 0.5, 0.2, 0.56),"cm"),
                             legend.position = c(.78, .88))
DBplot2 <- DBplot2 + theme(plot.margin = unit(c(0.2, 0.2, 0.58, 0.56),"cm"))
FigS1v2 <- plot_grid(DBplot2, NIAplot2, rel_widths = c(0.5, 1),
                     labels = "auto", label_size = 15)

FigS1v2 # 4 x 5 in


Splot <- Splot + theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.75),"cm"))
Cplot <- Cplot + theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.6),"cm"))
SMplot <- SMplot + theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.6),"cm"))
FigS2 <- plot_grid(Splot, Cplot, SMplot, nrow = 1, labels = "auto", label_size = 15)
FigS2 # 4 x 12 in

#### Clean up
detach(package:fitdistrplus)
detach(package:car)
detach(package:psych)
detach(package:cowplot)
detach(package:ggplot2)
rm(list = ls())
graphics.off()
cat("\014")
