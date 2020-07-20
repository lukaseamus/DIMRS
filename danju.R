########################################################################
#### Project: Danjugan Island Marine Reserve and Sanctuaries corals ####
#### Author: Luka Seamus Wright                                     ####
#### Date: 9th July 2020                                            ####
########################################################################

#### Univariate data
### Load data
danju <- read.csv("~/PATH/danju.csv")

### Data wrangling
## Response variables
HC <- danju$HC # Hard coral cover (%)
NIA <- danju$NIA # Macroalgal cover (%)
DIS <- danju$DIS # Diseased coral (%)
BL <- danju$FBL + danju$PBL # Bleached coral (%) (full + partial)
DC <- danju$DC + danju$RKC # Dead coral (%) (long + recent)
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
require(lme4)
require(car)

## Coral cover
# Build models
m1 <- lm(HC ~ site) # simple linear model
m2 <- lmer(HC ~ site + (1|depth), REML = F) # 1st mixed effects model:
# random intercept for depth factor
m3 <- lmer(HC ~ site + (1|site:depth), REML = F) # 2nd mixed effects model:
# random intercept for depth factor within fixed site factor

# Compare models
anova(m3, m2, m1) # models are not different
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
m4 <- lm(NIA ~ site)
m5 <- lmer(NIA ~ site + (1|depth), REML = F)
m6 <- lmer(NIA ~ site + (1|site:depth), REML = F)

# Compare models
anova(m5, m6, m4) # m6 is the best model
# -> proceed with m6
m6 <- lmer(NIA ~ site + (1|site:depth), REML = T)

# Test model fit
plot(m6) # quite homogenous

par(mfrow = c(1,2), mar = c(2,2,2,1))

hist(resid(m6)) # residuals are somewhat right skewed
qqnorm(resid(m6))
qqline(resid(m6))

par(mfrow = c(1,1), mar = c(5, 4, 4, 2) + 0.1)
# overall m6 fits the data ok, but fit could be improved

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
m7 <- glm(NIA+1 ~ site, family = Gamma(link = "inverse"))
m8 <- glmer(NIA+1 ~ site + (1|depth), family = Gamma(link = "inverse"))
m9 <- glmer(NIA+1 ~ site + (1|site:depth), family = Gamma(link = "inverse"))

# Compare models
anova(m8, m9, m7) # m7 is the best model
# -> proceed with m7

# Test model fit
par(mfrow = c(2,2), mar = c(2,2,2,1))
plot(m7) # quite homogenous

par(mfrow = c(1,2), mar = c(2,2,2,1))

hist(resid(m7)) # residuals are quite normally distributed
qqnorm(resid(m7))
qqline(resid(m7))

par(mfrow = c(1,1), mar = c(5, 4, 4, 2) + 0.1)
# overall m7 fits the data well
# goodness of fit is better than m6
# based on the better gamma fit, m7 is chosen as optimal

# Interpret model
Anova(m7, type = 2) # Type II sum of squares test
# Response: NIA + 1
#      LR Chisq Df Pr(>Chisq)    
# site   27.162  3  5.444e-06 ***

summary(m7) # pairwise contrasts
# Bamboo vs Manta; p = 0.04 *
# Bamboo vs Moray; p = 0.42
# Bamboo vs Tabon; p = 0.006 **

site <- factor(site, levels = c("Manta", "Moray", "Tabon", "Bamboo"))
m7 <- glm(NIA+1 ~ site, family = Gamma(link = "inverse"))
summary(m7)
# Manta vs Moray; p = 0.11
# Manta vs Tabon; p = 0.08

site <- factor(site, levels = c("Tabon", "Moray", "Manta", "Bamboo"))
m7 <- glm(NIA+1 ~ site, family = Gamma(link = "inverse"))
summary(m7)
# Tabon vs Moray; p = 0.01 *

site <- factor(site, levels = c("Bamboo", "Manta", "Moray", "Tabon"))


## Discoloured (diseased or bleached) coral
# Build models
m10 <- lm(DB ~ site)
m11 <- lmer(DB ~ site + (1|depth), REML = F)
m12 <- lmer(DB ~ site + (1|site:depth), REML = F)

# Compare models
anova(m11, m12, m10) # models are not different
# -> proceed with m10

# Test model fit
par(mfrow = c(2,2), mar = c(2,2,2,1))
plot(m10) # heterogenous

par(mfrow = c(1,2), mar = c(2,2,2,1))

hist(resid(m10)) # residuals are not normally distributed
qqnorm(resid(m10))
qqline(resid(m10))

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
m13 <- glm(DB+1 ~ site, family = Gamma(link = "inverse"))
m14 <- glmer(DB+1 ~ site + (1|depth), family = Gamma(link = "inverse"))
m15 <- glmer(DB+1 ~ site + (1|site:depth), family = Gamma(link = "inverse"))

# Compare models
anova(m14, m15, m13) # models are not different
# -> proceed with m13

# Test model fit
par(mfrow = c(2,2), mar = c(2,2,2,1))
plot(m13) # more homogenous

par(mfrow = c(1,2), mar = c(2,2,2,1))

hist(resid(m13)) # more normal
qqnorm(resid(m13))
qqline(resid(m13))

par(mfrow = c(1,1), mar = c(5, 4, 4, 2) + 0.1)
# overall m13 fits the data well
# goodness of fit is better than m10

# Interpret model
Anova(m13, type = 2) # Type II sum of squares test
# Response: DB + 1
#     LR Chisq Df Pr(>Chisq)    
# site   27.928  3  3.761e-06 ***

summary(m13) # pairwise contrasts
# Bamboo vs Manta; p = 0.019 *
# Bamboo vs Moray; p = 0.43
# Bamboo vs Tabon; p = 0.017 *

site <- factor(site, levels = c("Manta", "Moray", "Tabon", "Bamboo"))
m13 <- glm(DB+1 ~ site, family = Gamma(link = "inverse"))
summary(m13)
# Manta vs Moray; p = 0.03 *
# Manta vs Tabon; p = 0.81

site <- factor(site, levels = c("Tabon", "Moray", "Manta", "Bamboo"))
m13 <- glm(DB+1 ~ site, family = Gamma(link = "inverse"))
summary(m13)
# Tabon vs Moray; p = 0.03 *

site <- factor(site, levels = c("Bamboo", "Manta", "Moray", "Tabon"))


## Dead coral
# Build models
m16 <- lm(DC ~ site)
m17 <- lmer(DC ~ site + (1|depth), REML = F)
m18 <- lmer(DC ~ site + (1|site:depth), REML = F)

# Compare models
anova(m18, m17, m16) # models are not different
# -> proceed with m16

# Test model fit
par(mfrow = c(2,2), mar = c(2,2,2,1))
plot(m16) # quite homogenous

par(mfrow = c(1,2), mar = c(2,2,2,1))

hist(resid(m16)) # normality is ok
qqnorm(resid(m16))
qqline(resid(m16))

par(mfrow = c(1,1), mar = c(5, 4, 4, 2) + 0.1)


# Interpret model
Anova(m16, type = 2) # Type II sum of squares test
# Response: DC
#           Sum Sq Df F value    Pr(>F)    
# site      7445.2  3  8.0249 0.0005162 ***
# Residuals 8659.1 28 

summary(m16) # pairwise contrasts
# Bamboo vs Manta; p < 0.001 ***
# Bamboo vs Moray; p = 0.04 *
# Bamboo vs Tabon; p < 0.001 ***

site <- factor(site, levels = c("Manta", "Moray", "Tabon", "Bamboo"))
m16 <- lm(DC ~ site)
summary(m16)
# Manta vs Moray; p = 0.04 *
# Manta vs Tabon; p = 0.93

site <- factor(site, levels = c("Tabon", "Moray", "Manta", "Bamboo"))
m16 <- lm(DC ~ site)
summary(m16)
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
permutest(betad) # heterogenous

## Transform
sqrt.coral <- sqrt(coral)

## Calculate Bray-Curtis dissimilarity
coral.dist <- vegdist(sqrt.coral, method = "bray")

## Test for homogeneity of dispersion
betad <- with(env, betadisper(coral.dist, site))
permutest(betad) # homogenous

## PERMANOVA, "morphospecies" abundance data
adonis(sqrt.coral ~ site, env, permutations = 9999)
#           Df SumsOfSqs MeanSqs F.Model      R2 Pr(>F)    
# site       3    1.4676 0.48919  5.4988 0.37074  1e-04 ***
# Residuals 28    2.4909 0.08896         0.62926           
# Total     31    3.9585                 1.00000 

# site significantly affects the community composition of 
# "morphospecies" and explains ~37% of the variance in 
# community composition

## SIMPER, "morphospecies" influence on dissimilarity between sites
morphospecies <- with(env, simper(sqrt.coral, site, permutations = 9999))
summary(morphospecies, ordered = T)
# corymbose coral seems to be a major driver of dissimilarity
# -> worth looking into some univariate analysis

## Corymbose coral (%)
# Build models
C <- danju$C
m19 <- lm(C ~ site)
m20 <- lmer(C ~ site + (1|depth), REML = F)
m21 <- lmer(C ~ site + (1|site:depth), REML = F)

# Compare models
anova(m21, m20, m19) # m21 is the best model
# -> proceed with m21
m21 <- lmer(C ~ site + (1|site:depth), REML = T)

# Test model fit
plot(m21) # quite homogenous

par(mfrow = c(1,2), mar = c(2,2,2,1))

hist(resid(m21)) # residuals are normally distributed
qqnorm(resid(m21))
qqline(resid(m21))

par(mfrow = c(1,1), mar = c(5, 4, 4, 2) + 0.1)

# Interpret model
Anova(m21, type = 2) # Type II sum of squares test
# Response: C
#       Chisq Df Pr(>Chisq)   
# site 13.712  3   0.003324 **  

confint(m21) # pairwise contrasts
# Bamboo vs Manta; p > 0.05
# Bamboo vs. Moray; p < 0.05 *
# Bamboo vs. Tabon; p > 0.05

site <- factor(site, levels = c("Manta", "Moray", "Tabon", "Bamboo"))
m21 <- lmer(C ~ site + (1|site:depth), REML = T)
confint(m21)
# Manta vs Moray; p < 0.05 *
# Manta vs Tabon; p > 0.05

site <- factor(site, levels = c("Tabon", "Moray", "Manta", "Bamboo"))
m21 <- lmer(C ~ site + (1|site:depth), REML = T)
confint(m21)
# Tabon vs Moray; p < 0.05 *

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
m22 <- lm(S ~ site)
m23 <- lmer(S ~ site + (1|depth), REML = F)
m24 <- lmer(S ~ site + (1|site:depth), REML = F)

# Compare models
anova(m24, m23, m22) # models are not different
# -> proceed with m22

# Test model fit
par(mfrow = c(2,2), mar = c(2,2,2,1))
plot(m22) # quite homogenous

par(mfrow = c(1,2), mar = c(2,2,2,1))

hist(resid(m22)) # residuals are quite normally distributed
qqnorm(resid(m22))
qqline(resid(m22))

par(mfrow = c(1,1), mar = c(5, 4, 4, 2) + 0.1)

# Interpret model
Anova(m22, type = 2) # Type II sum of squares test
# Response: S
#           Sum Sq Df F value   Pr(>F)   
# site      23.125  3  5.2965 0.005094 **
# Residuals 40.750 28     

summary(m22) # pairwise contrasts
# Bamboo vs Manta; p = 0.11
# Bamboo vs. Moray; p = 0.84
# Bamboo vs. Tabon; p = 0.03 *

site <- factor(site, levels = c("Manta", "Moray", "Tabon", "Bamboo"))
m22 <- lm(S ~ site)
summary(m22)
# Manta vs Moray; p = 0.16
# Manta vs Tabon; p < 0.001 ***

site <- factor(site, levels = c("Tabon", "Moray", "Manta", "Bamboo"))
m22 <- lm(S ~ site)
summary(m22)
# Tabon vs Moray; p = 0.02 *

site <- factor(site, levels = c("Bamboo", "Manta", "Moray", "Tabon"))


## Simpson's evenness
# Build models
m25 <- lm(E ~ site)
m26 <- lmer(E ~ site + (1|depth), REML = F)
m27 <- lmer(E ~ site + (1|site:depth), REML = F)

# Compare models
anova(m26, m27, m25) # models are not different
# -> proceed with m25

# Test model fit
par(mfrow = c(2,2), mar = c(2,2,2,1))
plot(m25) # quite homogenous

par(mfrow = c(1,2), mar = c(2,2,2,1))

hist(resid(m25)) # residuals are quite normally distributed
qqnorm(resid(m25))
qqline(resid(m25))

par(mfrow = c(1,1), mar = c(5, 4, 4, 2) + 0.1)

# Interpret model
Anova(m25, type = 2) # Type II sum of squares test
# Response: E
#           Sum Sq Df F value    Pr(>F)    
# site      0.95375  3  22.052 1.558e-07 ***
# Residuals 0.40368 28     

summary(m25) # pairwise contrasts
# Bamboo vs Manta; p < 0.001 ***
# Bamboo vs. Moray; p < 0.001 ***
# Bamboo vs. Tabon; p = 0.81

site <- factor(site, levels = c("Manta", "Moray", "Tabon", "Bamboo"))
m25 <- lm(E ~ site)
summary(m25)
# Manta vs Moray; p = 0.94
# Manta vs Tabon; p < 0.001 ***

site <- factor(site, levels = c("Tabon", "Moray", "Manta", "Bamboo"))
m25 <- lm(E ~ site)
summary(m25)
# Tabon vs Moray; p < 0.001 ***

site <- factor(site, levels = c("Bamboo", "Manta", "Moray", "Tabon"))


### Data visualisation
## Calculate nMDS data
coralmds <- metaMDS(sqrt.coral, distance = "bray", autotransform = FALSE)
coralmds$stress # stress = 0.18

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
                  annotate("text", label = "Stress = 0.18", x = -0.749, y = 0.54,
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
                        panel.border = element_rect(fill = NA, size = 1)) +
                  coord_cartesian(y = c(-0.8, 1.1))

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
                            label = c("a","a","b","a"))) +
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
              ylab("Number of morphologies") +
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
detach(package:lme4)
detach(package:car)
detach(package:psych)
detach(package:ggplot2)
rm(list = ls())
graphics.off()
cat("\014")
