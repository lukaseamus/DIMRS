########################################################################
#### Project: Danjugan Island Marine Reserve and Sanctuaries corals ####
#### Author: Luka Seamus Wright                                     ####
#### Date: 9th July 2020                                            ####
########################################################################

#### Univariate data
### Load data
danju <- read.csv("~/Desktop/Projects/Danjugan/danju.csv")

### Data wrangling
## Response variables
HC <- danju$percHC # Hard coral cover (%)
NIA <- danju$percNIA # Macroalgal cover (%)
DIS <- danju$percDIS # Diseased coral (%)
BL <- danju$percFBL + danju$percPBL # Bleached coral (%) (full + partial)
DC <- danju$percDC + danju$percRKC # Dead coral (%) (long + recent)
DB <- DIS + BL # Diseased or bleached coral (%)

## Explanatory variables
site <- danju$site
depth <- danju$depth

### Data exploration
require(fitdistrplus)

## Response variables
par(mfrow = c(3,1), mar = c(2,2,2,1))

hist(HC)
boxplot(HC, horizontal = T)
descdist(HC, boot = 500)
# almost uniform

hist(NIA)
boxplot(NIA, horizontal = T)
descdist(NIA, boot = 500)
# right skewed

hist(DIS)
boxplot(DIS, horizontal = T)
descdist(DIS, boot = 500)
# right skewed

hist(BL)
boxplot(BL, horizontal = T)
descdist(BL, boot = 500)
# right skewed

hist(DC)
boxplot(DC, horizontal = T)
descdist(DC, boot = 500)
# close to normal

hist(DB)
boxplot(DB, horizontal = T)
descdist(DB, boot = 500)
# right skewed but not as bad as DIS or BL -> continue with combination

par(mfrow = c(1,1), mar = c(5, 4, 4, 2) + 0.1)

## Explanatory and response variables
boxplot(HC ~ site) # homogenous
boxplot(NIA ~ site) # heterogenous
boxplot(DIS ~ site) # heterogenous
boxplot(BL ~ site) # heterogenous
boxplot(DC ~ site) # homogenous
boxplot(DB ~ site) # heterogenous

### Data analysis
require(nlme)
require(car)

## Coral cover
# Build models
m1 <- lm(HC ~ site) # simple linear model
m2 <- lme(HC ~ site, random = ~1|depth, method = "ML") # mixed effects model

# Compare models
anova(m2, m1) # models are not different
# -> proceed with m1

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


## Macroalgal cover
# Build models
m3 <- lm(NIA ~ site)
m4 <- lme(NIA ~ site, random = ~1|depth, method = "ML")

# Compare models
anova(m4, m3) # m4 is the better model
# -> proceed with m4
m4 <- lme(NIA ~ site, random = ~1|depth, method = "REML")

# Test model fit
plot(m4) # quite homogenous

par(mfrow = c(1,2), mar = c(2,2,2,1))

hist(resid(m4)) # residuals are quite normally distributed
qqnorm(resid(m4))
qqline(resid(m4))

par(mfrow = c(1,1), mar = c(5, 4, 4, 2) + 0.1)
# overall m4 fits the data well, but homogeneity could be improved

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
require(lme4)
m5 <- glm(NIA+1 ~ site, family = Gamma(link = "inverse"))
m6 <- glmer(NIA+1 ~ site + (1|depth), family = Gamma(link = "inverse"))

# Compare models
anova(m6, m5) # models are not different
# -> proceed with m5

# Test model fit
par(mfrow = c(2,2), mar = c(2,2,2,1))
plot(m5) # quite homogenous

par(mfrow = c(1,2), mar = c(2,2,2,1))

hist(resid(m5)) # residuals are quite normally distributed
qqnorm(resid(m5))
qqline(resid(m5))

par(mfrow = c(1,1), mar = c(5, 4, 4, 2) + 0.1)
# overall m5 fits the data well
# goodness of fit is quite similar to m4
# based on the better gamma fit, m5 is chosen as optimal

# Interpret model
Anova(m5, type = 2) # Type II sum of squares test
# Response: NIA + 1
#      LR Chisq Df Pr(>Chisq)    
# site   27.162  3  5.444e-06 ***

summary(m5) # pairwise contrasts
# Bamboo vs Manta; p = 0.04 *
# Bamboo vs Moray; p = 0.42
# Bamboo vs Tabon; p = 0.006 **

site <- factor(site, levels = c("Manta", "Moray", "Tabon", "Bamboo"))
m5 <- glm(NIA+1 ~ site, family = Gamma(link = "inverse"))
summary(m5)
# Manta vs Moray; p = 0.11
# Manta vs Tabon; p = 0.08

site <- factor(site, levels = c("Tabon", "Moray", "Manta", "Bamboo"))
m5 <- glm(NIA+1 ~ site, family = Gamma(link = "inverse"))
summary(m5)
# Tabon vs Moray; p = 0.01 *

site <- factor(site, levels = c("Bamboo", "Manta", "Moray", "Tabon"))


## Discoloured (diseased or bleached) coral
# Build models
m7 <- lm(DB ~ site)
m8 <- lme(DB ~ site, random = ~1|depth, method = "ML")

# Compare models
anova(m8, m7) # models are not different
# -> proceed with m7

# Test model fit
par(mfrow = c(2,2), mar = c(2,2,2,1))
plot(m7) # heterogenous

par(mfrow = c(1,2), mar = c(2,2,2,1))

hist(resid(m7)) # residuals are not normally distributed
qqnorm(resid(m7))
qqline(resid(m7))

par(mfrow = c(1,1), mar = c(5, 4, 4, 2) + 0.1)

# homogeneity of variance and normality should be imporved
# try fitting gamma distribution to data
gamma <- fitdist(DB+1, "gamma")
norm <- fitdist(DB+1, "norm")

cdfcomp(list(gamma, norm), 
        legendtext = c("Gamma", "Normal"), 
        fitlty = 1)

gofstat(list(gamma, norm), 
        fitnames = c("Gamma", "Normal"))
# gamma fits better

# Build gamma models
m9 <- glm(DB+1 ~ site, family = Gamma(link = "inverse"))
m10 <- glmer(DB+1 ~ site + (1|depth), family = Gamma(link = "inverse"))

# Compare models
anova(m10, m9) # models are not different
# -> proceed with m9

# Test model fit
par(mfrow = c(2,2), mar = c(2,2,2,1))
plot(m9) # more homogenous

par(mfrow = c(1,2), mar = c(2,2,2,1))

hist(resid(m9)) # more normal
qqnorm(resid(m9))
qqline(resid(m9))

par(mfrow = c(1,1), mar = c(5, 4, 4, 2) + 0.1)
# overall m9 fits the data well
# goodness of fit is better than m8

# Interpret model
Anova(m9, type = 2) # Type II sum of squares test
# Response: DB + 1
#     LR Chisq Df Pr(>Chisq)    
# site   27.928  3  3.761e-06 ***

summary(m9) # pairwise contrasts
# Bamboo vs Manta; p = 0.019 *
# Bamboo vs Moray; p = 0.43
# Bamboo vs Tabon; p = 0.017 *

site <- factor(site, levels = c("Manta", "Moray", "Tabon", "Bamboo"))
m9 <- glm(DB+1 ~ site, family = Gamma(link = "inverse"))
summary(m9)
# Manta vs Moray; p = 0.03 *
# Manta vs Tabon; p = 0.81

site <- factor(site, levels = c("Tabon", "Moray", "Manta", "Bamboo"))
m9 <- glm(DB+1 ~ site, family = Gamma(link = "inverse"))
summary(m9)
# Tabon vs Moray; p = 0.03 *

site <- factor(site, levels = c("Bamboo", "Manta", "Moray", "Tabon"))


## Dead coral
# Build models
m11 <- lm(DC ~ site)
m12 <- lme(DC ~ site, random = ~1|depth, method = "ML")

# Compare models
anova(m12, m11) # models are not different
# -> proceed with m11

# Test model fit
par(mfrow = c(2,2), mar = c(2,2,2,1))
plot(m11) # quite homogenous

par(mfrow = c(1,2), mar = c(2,2,2,1))

hist(resid(m11)) # normality is ok
qqnorm(resid(m11))
qqline(resid(m11))

par(mfrow = c(1,1), mar = c(5, 4, 4, 2) + 0.1)


# Interpret model
Anova(m11, type = 2) # Type II sum of squares test
# Response: DC
#           Sum Sq Df F value    Pr(>F)    
# site      7445.2  3  8.0249 0.0005162 ***
# Residuals 8659.1 28 

summary(m11) # pairwise contrasts
# Bamboo vs Manta; p < 0.001 ***
# Bamboo vs Moray; p = 0.04 *
# Bamboo vs Tabon; p < 0.001 ***

site <- factor(site, levels = c("Manta", "Moray", "Tabon", "Bamboo"))
m11 <- lm(DC ~ site)
summary(m11)
# Manta vs Moray; p = 0.04 *
# Manta vs Tabon; p = 0.93

site <- factor(site, levels = c("Tabon", "Moray", "Manta", "Bamboo"))
m11 <- lm(DC ~ site)
summary(m11)
# Tabon vs Moray; p = 0.052

site <- factor(site, levels = c("Bamboo", "Manta", "Moray", "Tabon"))


### Data visualisation
## Calculate descriptive statistics
require(psych)

HCstat <- describeBy(HC, site, mat = T, digits = 3)
NIAstat <- describeBy(NIA, site, mat = T, digits = 3)
DBstat <- describeBy(DB, site, mat = T, digits = 3)
DCstat <- describeBy(DC, site, mat = T, digits = 3)

## Reorder sites
HCstat$group1 <- factor(HCstat$group1, levels = c("Manta", "Moray", "Tabon", "Bamboo"))
NIAstat$group1 <- factor(NIAstat$group1, levels = c("Manta", "Moray", "Tabon", "Bamboo"))
DBstat$group1 <- factor(DBstat$group1, levels = c("Manta", "Moray", "Tabon", "Bamboo"))
DCstat$group1 <- factor(DCstat$group1, levels = c("Manta", "Moray", "Tabon", "Bamboo"))

## Set theme
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
                 legend.title = element_text(size = 12, face = "bold"),
                 text = element_text(family = "Helvetica Neue"))

## Plot
# Coral cover
HCplot <- ggplot(HCstat, mapping = aes(group1, mean)) +
              geom_col(width = 0.7, fill = c("#f7b060" , "#467289", "#7fb8d5", "#ef8407")) +
              geom_errorbar(mapping = aes(ymin = mean - se, ymax = mean + se),
                            width = .1, lwd = .4) +
              geom_text(aes(x = group1, y = mean + se + 1.7,
                            label = c("b","a","a","b"))) +
              ylab("Scleractinian coral cover (%)") +
              coord_cartesian(ylim = c(3.33, 70)) +
              mytheme
HCplot # dimensions: 4.5 x 4 in

# Macroalgal cover
NIAplot <- ggplot(NIAstat, mapping = aes(group1, mean)) +
              geom_col(width = 0.7, fill = c("#f7b060" , "#467289", "#7fb8d5", "#ef8407")) +
              geom_errorbar(mapping = aes(ymin = mean - se, ymax = mean + se),
                            width = .1, lwd = .4) +
              geom_text(aes(x = group1, y = mean + se + 1.2,
                            label = c("b","a","ab","a"))) +
              ylab("Macroalgal cover (%)") +
              coord_cartesian(ylim = c(2.38, 50)) +
              mytheme
NIAplot # dimensions: 4.5 x 4 in

# Discoloured coral
DBplot <- ggplot(DBstat, mapping = aes(group1, mean)) +
              geom_col(width = 0.7, fill = c("#f7b060" , "#467289", "#7fb8d5", "#ef8407")) +
              geom_errorbar(mapping = aes(ymin = mean - se, ymax = mean + se),
                            width = .1, lwd = .4) +
              geom_text(aes(x = group1, y = mean + se + 1,
                            label = c("a","b","a","b"))) +
              ylab("Discoloured scleractinian coral (%)") +
              coord_cartesian(ylim = c(1.95, 41)) +
              mytheme
DBplot # dimensions: 4.5 x 4 in

# Dead coral
DCplot <- ggplot(DCstat, mapping = aes(group1, mean)) +
              geom_col(width = 0.7, fill = c("#f7b060" , "#467289", "#7fb8d5", "#ef8407")) +
              geom_errorbar(mapping = aes(ymin = mean - se, ymax = mean + se),
                            width = .1, lwd = .4) +
              geom_text(aes(x = group1, y = mean + se + 1.6,
                            label = c("ab","b","a","c"))) +
              ylab("Dead scleractinian coral (%)") +
              coord_cartesian(ylim = c(3.23, 68)) +
              mytheme
DCplot # dimensions: 4.5 x 4 in


################################################################################################


#### Multivariate "morphospecies" data
require(vegan)

### Load data
morph <- danju[,c(1:6, 24:33)]
str(morph)

### Data wrangling
## Reorder levels of site factor
morph$site <- as.character(morph$site)
morph$site <- factor(morph$site, levels = c("Manta", "Moray", "Tabon", "Bamboo"))
str(morph)

## Define environmental variables
env <- morph[,1:5]

## Define "morphospecies" abundance data
coral <- morph[,6:16]
coral <- data.frame(coral, row.names = 1)

### Data analysis
## Calculate Bray-Curtis dissimilarity
coral.dist <- vegdist(coral, method = "bray")

## Test for homogeneity of dispersion
betad <- with(env, betadisper(coral.dist, site))
permutest(betad) # homogenous

## PERMANOVA, "morphospecies" abundance data
adonis(coral ~ site, env, permutations = 9999)
#           Df SumsOfSqs MeanSqs F.Model      R2 Pr(>F)    
# site       3    2.6061 0.86869  6.4142 0.40731  1e-04 ***
# Residuals 28    3.7922 0.13543         0.59269           
# Total     31    6.3982                 1.00000 

# site significantly affects the community composition of 
# "morphospecies" and explains ~41% of the variance in 
# community composition

## SIMPER, "morphospecies" influence on dissimilarity between sites
morphospecies <- with(env, simper(coral, site, permutations = 9999))
summary(morphospecies, ordered = T)
# corymbose coral seems to be the main driver of dissimilarity and
# is generally more abundant at protected sites
# however, this could simply be due to it being most abundant 
# -> worth looking into some univariate analysis

## Corymbose coral (%)
# Build models
C <- danju$percC
m13 <- lm(C ~ site)
m14 <- lme(C ~ site, random = ~1|depth, method = "ML")

# Compare models
anova(m14, m13) # models are not different
# -> proceed with m13

# Test model fit
par(mfrow = c(2,2), mar = c(2,2,2,1))
plot(m13) # quite homogenous

par(mfrow = c(1,2), mar = c(2,2,2,1))

hist(resid(m13)) # residuals are normally distributed
qqnorm(resid(m13))
qqline(resid(m13))

par(mfrow = c(1,1), mar = c(5, 4, 4, 2) + 0.1)

# Interpret model
Anova(m13, type = 2) # Type II sum of squares test
# Response: C
#            Sum Sq Df F value    Pr(>F)    
# site      12095.9  3  16.597 2.164e-06 ***
# Residuals  6802.2 28    

summary(m13) # pairwise contrasts
# Bamboo vs Manta; p = 0.42
# Bamboo vs. Moray; p < 0.001 ***
# Bamboo vs. Tabon; p = 0.07

site <- factor(site, levels = c("Manta", "Moray", "Tabon", "Bamboo"))
m13 <- lm(C ~ site)
summary(m13)
# Manta vs Moray; p < 0.001 ***
# Manta vs Tabon; p = 0.01 *

site <- factor(site, levels = c("Tabon", "Moray", "Manta", "Bamboo"))
m13 <- lm(C ~ site)
summary(m13)
# Tabon vs Moray; p < 0.001 ***

site <- factor(site, levels = c("Bamboo", "Manta", "Moray", "Tabon"))

## Calculate diversity indices
S <- specnumber(coral) # Species richness
D <- diversity(coral, index = "invsimpson", base = exp(1)) # Simpson's reciprocal index (1/D)
D2 <- diversity(coral, index = "simpson", base = exp(1)) # Simpson's diversity index (1-D)
H <- diversity(coral, index = "shannon", base = exp(1)) # Shannon-Wiener diversity index
E <- D/S # Simpson's evenness
J <- H/log(S) # Pielou's evenness
J # NaN produced -> proceed with E instead of J

## "Morphospecies" richness
# Build models
m15 <- lm(S ~ site)
m16 <- lme(S ~ site, random = ~1|depth, method = "ML")

# Compare models
anova(m16, m15) # models are not different
# -> proceed with m15

# Test model fit
par(mfrow = c(2,2), mar = c(2,2,2,1))
plot(m15) # quite homogenous

par(mfrow = c(1,2), mar = c(2,2,2,1))

hist(resid(m15)) # residuals are quite normally distributed
qqnorm(resid(m15))
qqline(resid(m15))

par(mfrow = c(1,1), mar = c(5, 4, 4, 2) + 0.1)

# Interpret model
Anova(m15, type = 2) # Type II sum of squares test
# Response: S
#           Sum Sq Df F value   Pr(>F)   
# site      23.125  3  5.2965 0.005094 **
# Residuals 40.750 28     

summary(m15) # pairwise contrasts
# Bamboo vs Manta; p = 0.11
# Bamboo vs. Moray; p = 0.84
# Bamboo vs. Tabon; p = 0.03 *

site <- factor(site, levels = c("Manta", "Moray", "Tabon", "Bamboo"))
m15 <- lm(S ~ site)
summary(m15)
# Manta vs Moray; p = 0.16
# Manta vs Tabon; p < 0.001 ***

site <- factor(site, levels = c("Tabon", "Moray", "Manta", "Bamboo"))
m15 <- lm(S ~ site)
summary(m15)
# Tabon vs Moray; p = 0.02 *

site <- factor(site, levels = c("Bamboo", "Manta", "Moray", "Tabon"))


## Simpson's evenness
# Build models
m17 <- lm(E ~ site)
m18 <- lme(E ~ site, random = ~1|depth, method = "ML")

# Compare models
anova(m18, m17) # models are not different
# -> proceed with m17

# Test model fit
par(mfrow = c(2,2), mar = c(2,2,2,1))
plot(m17) # quite homogenous

par(mfrow = c(1,2), mar = c(2,2,2,1))

hist(resid(m17)) # residuals are quite normally distributed
qqnorm(resid(m17))
qqline(resid(m17))

par(mfrow = c(1,1), mar = c(5, 4, 4, 2) + 0.1)

# Interpret model
Anova(m17, type = 2) # Type II sum of squares test
# Response: E
#           Sum Sq Df F value    Pr(>F)    
# site      0.95375  3  22.052 1.558e-07 ***
# Residuals 0.40368 28     

summary(m17) # pairwise contrasts
# Bamboo vs Manta; p < 0.001 ***
# Bamboo vs. Moray; p < 0.001 ***
# Bamboo vs. Tabon; p = 0.81

site <- factor(site, levels = c("Manta", "Moray", "Tabon", "Bamboo"))
m17 <- lm(E ~ site)
summary(m17)
# Manta vs Moray; p = 0.94
# Manta vs Tabon; p < 0.001 ***

site <- factor(site, levels = c("Tabon", "Moray", "Manta", "Bamboo"))
m17 <- lm(E ~ site)
summary(m17)
# Tabon vs Moray; p < 0.001 ***

site <- factor(site, levels = c("Bamboo", "Manta", "Moray", "Tabon"))


### Data visualisation
## Calculate nMDS data
coralmds <- metaMDS(coral, distance = "bray", autotransform = FALSE)
coralmds$stress # stress = 0.15

## Calculate data points and 95% confidence ellipses
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

## Define colours, shapes and factor level order
colours <- c("#7fb8d5", "#467289", "#f7b060", "#ef8407")
shapes <- c(21:24)
df$group <- factor(df$group, levels = c("Manta", "Moray", "Tabon", "Bamboo"))

## Plot
morph.plot <- ggplot(data = df, aes(MDS1, MDS2)) +
                  geom_point(aes(colour = group, shape = group, fill = group),
                             size = 3) +
                  geom_polygon(data = dfell, aes(NMDS1, NMDS2, colour = group,
                                   fill = group), size = 0.7, alpha = 0.3) +
                  scale_fill_manual(values = colours,
                                    labels = c("Manta", "Moray", "Tabon", "Bamboo"),
                                    guide = guide_legend(title = "Site")) +
                  annotate("text", label = "Stress = 0.15", x = -0.769, y = 0.8,
                           size = 4.2) +
                  scale_colour_manual(values = colours,
                                      labels = c("Manta", "Moray", "Tabon", "Bamboo"),
                                      guide = guide_legend(title = "Site")) +
                  scale_shape_manual(values = shapes,
                                     labels = c("Manta", "Moray", "Tabon", "Bamboo"),
                                     guide = guide_legend(title = "Site")) +
                  mytheme +
                  theme(legend.position = c(0.15, 0.86),
                        legend.title = element_blank(),
                        axis.title = element_blank(),
                        axis.ticks = element_blank(),
                        axis.text = element_blank(),
                        axis.line = element_blank(),
                        panel.border = element_rect(fill = NA, size = 1))

morph.plot # dimensions: 4.5 x 4.5 in


## Calculate unvariate descriptive statistics
Cstat <- describeBy(C, site, mat = T, digits = 3)
Sstat <- describeBy(S, site, mat = T, digits = 3)
Estat <- describeBy(E, site, mat = T, digits = 3)

## Reorder site factor levels
Cstat$group1 <- factor(Cstat$group1, levels = c("Manta", "Moray", "Tabon", "Bamboo"))
Sstat$group1 <- factor(Sstat$group1, levels = c("Manta", "Moray", "Tabon", "Bamboo"))
Estat$group1 <- factor(Estat$group1, levels = c("Manta", "Moray", "Tabon", "Bamboo"))

## Plot
# Corymbose coral
Cplot <- ggplot(Cstat, mapping = aes(group1, mean)) +
              geom_col(width = 0.7, fill = c("#ef8407", "#7fb8d5", "#467289", "#f7b060")) +
              geom_errorbar(mapping = aes(ymin = mean - se, ymax = mean + se),
                            width = .1, lwd = .4) +
              geom_text(aes(x = group1, y = mean + se + 1.8,
                            label = c("ac","a","b","c"))) +
              ylab("Corymbose scleractinian coral (%)") +
              coord_cartesian(ylim = c(3.57, 75)) +
              mytheme
Cplot # dimensions: 4.5 x 4 in


# "Morphospecies" richness
Splot <- ggplot(Sstat, mapping = aes(group1, mean)) +
              geom_col(width = 0.7, fill = c("#ef8407", "#7fb8d5", "#467289", "#f7b060")) +
              geom_errorbar(mapping = aes(ymin = mean - se, ymax = mean + se),
                            width = .1, lwd = .4) +
              geom_text(aes(x = group1, y = mean + se + 0.15,
                            label = c("a","a","a","b"))) +
              ylab("Morphospecies richness") +
              coord_cartesian(ylim = c(0.285, 6)) +
              mytheme
Splot # dimensions: 4.5 x 4 in

# Simpson's evenness
Eplot <- ggplot(Estat, mapping = aes(group1, mean)) +
              geom_col(width = 0.7, fill = c("#ef8407", "#7fb8d5", "#467289", "#f7b060")) +
              geom_errorbar(mapping = aes(ymin = mean - se, ymax = mean + se),
                            width = .1, lwd = .4) +
              geom_text(aes(x = group1, y = mean + se + 0.025,
                            label = c("b","a","a","b"))) +
              ylab("Simpson's evenness index") +
              coord_cartesian(ylim = c(0.0476, 1)) +
              mytheme
Eplot # dimensions: 4.5 x 4.14 in



#### Multivariate substratum data
### Load data
sub <- danju[,1:16]
str(sub)

### Data wrangling
## Reorder levels of site factor
sub$site <- as.character(sub$site)
sub$site <- factor(sub$site, levels = c("Manta", "Moray", "Tabon", "Bamboo"))
str(sub)

## Define environmental variables
env <- sub[,1:5]

## Define species abundance data
subtyp <- sub[,6:16]
subtyp <- data.frame(subtyp, row.names = 1)

### Data analysis
## Calculate Bray-Curtis dissimilarity
sub.dist <- vegdist(subtyp, method = "bray")

## Test for homogeneity of dispersion
betad <- with(env, betadisper(sub.dist, site))
permutest(betad) # homogenous

## PERMANOVA, substratum quantity data
adonis(subtyp ~ site, env, permutations = 9999)
#           Df SumsOfSqs MeanSqs F.Model      R2 Pr(>F)    
# site       3    2.6534 0.88446  13.587 0.59279  1e-04 ***
# Residuals 28    1.8227 0.06510         0.40721           
# Total     31    4.4761                 1.00000 

# site significantly affects the substratum composition
# and explains ~59% of the variance in substratum composition

## SIMPER, substratum influence on dissimilarity between sites
substrata <- with(env, simper(subtyp, site, permutations = 9999))
summary(substrata, ordered = T)
# scleratinian coral seems to be the main driver of dissimilarity,
# especially between protected and unprotected sites

### Data visualisation
## Calculate nMDS data
submds <- metaMDS(subtyp, autotransform = FALSE)
submds$stress # stress = 0.06

## Calculate data points and 95% confidence ellipses
df <- data.frame(submds$points, group = site)
ellipse <- ordiellipse(submds, site, display = "sites", kind = "se",
                       conf = 0.95, label = F)

## Make data compatible with ggplot2
dfell <- data.frame()

for(g in levels(df$group)) {
  dfell <- rbind(dfell, 
                 cbind(as.data.frame(with(df[df$group==g,],
                                          veganCovEllipse(ellipse[[g]]$cov,
                                                          ellipse[[g]]$center,
                                                          ellipse[[g]]$scale))),
                       group = g))
}

## Define factor level order
df$group <- factor(df$group, levels = c("Manta", "Moray", "Tabon", "Bamboo"))

## Plot
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
                        legend.title = element_blank(),
                        axis.title = element_blank(),
                        axis.ticks = element_blank(),
                        axis.text = element_blank(),
                        axis.line = element_blank(),
                        panel.border = element_rect(fill = NA, size = 1))

sub.plot # dimensions: 4.5 x 4.5 in


#### Clean up
detach(package:fitdistrplus)
detach(package:car)
detach(package:nlme)
detach(package:lme4)
detach(package:psych)
detach(package:ggplot2)
detach(package:emmeans)
rm(list = ls())
graphics.off()
cat("\014")
