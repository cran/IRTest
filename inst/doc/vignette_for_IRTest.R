## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(IRTest)
library(ggplot2)

## -----------------------------------------------------------------------------
Alldata <- DataGeneration(seed = 123456789,
                          model_D = rep(1:2, each=5),
                          N=1000,
                          nitem_D = 10,
                          nitem_P = 0,
                          d = 1.664,
                          sd_ratio = 2,
                          prob = 0.3)

data <- Alldata$data_D
item <- Alldata$item_D
initialitem <- Alldata$initialitem_D
theta <- Alldata$theta

## ---- fig.align='center', fig.height=2.5, fig.width=6, echo=F-----------------
ggplot(data = data.frame(x = c(-6,6)), aes(x)) +
  stat_function(fun = dist2, n = 101, args = list(prob = 0.3, d=1.664, sd_ratio = 2)) +
  ylab("latent density") +
  xlab(expression(theta)) +
  scale_y_continuous(breaks = NULL)

## ---- results='hide'----------------------------------------------------------
######                            ######
###### Empirical histogram method ######
######                            ######
Mod1 <- IRTest_Dich(initialitem = initialitem,
                    data = data,
                    model = rep(1:2, each=5),
                    latent_dist = "EHM",
                    max_iter = 200,
                    threshold = .0001)

######                                  ######
###### Kernel density estimation method ######
######                                  ######
# Mod1 <- IRTest_Dich(initialitem = initialitem,
#                     data = data,
#                     model = rep(1:2, each=5),
#                     latent_dist = "KDE",
#                     bandwidth = "SJ-ste",
#                     max_iter = 200,
#                     threshold = .001)

######                      ######
###### Normality assumption ######
######                      ######
#  Mod1 <- IRTest_Dich(initialitem = initialitem,
#                      data = data,
#                      model = rep(1:2, each=5),
#                      latent_dist = "Normal",
#                      max_iter = 200,
#                      threshold = .0001)

######                                             ######
###### Two-component Gaussian mixture distribution ######
######                                             ######
#  Mod1 <- IRTest_Dich(initialitem = initialitem,
#                      data = data,
#                      model = rep(1:2, each=5),
#                      latent_dist = "Mixture",
#                      max_iter = 200,
#                      threshold = .0001)

######                                                       ######
###### Davidian curve (for an arbitrarily chosen case of h=4)######
######                                                       ######
#  Mod1 <- IRTest_Dich(initialitem = initialitem,
#                      data = data,
#                      model = rep(1:2, each=5),
#                      latent_dist = "DC",
#                      max_iter = 200,
#                      threshold = .0001,
#                      h=4)

## ---- fig.align='center', fig.height=2.5, fig.width=6-------------------------
### The estimated item parameters
Mod1$par_est

### The asymptotic standard errors of item parameters
Mod1$se

### The estimated ability parameters
head(Mod1$theta)

### The estimated latent distribution
plot_LD(Mod1, xlim = c(-6,6))

## -----------------------------------------------------------------------------
Alldata <- DataGeneration(seed = 123456789,
                          model_P = "GPCM",
                          categ = rep(c(3,7), each = 5),
                          N=1000,
                          nitem_D = 0,
                          nitem_P = 10,
                          d = 1.414,
                          sd_ratio = 2,
                          prob = 0.5)

data <- Alldata$data_P
item <- Alldata$item_P
initialitem <- Alldata$initialitem_P
theta <- Alldata$theta

## ---- fig.align='center', fig.height=2.5, fig.width=6, echo=F-----------------
ggplot(data = data.frame(x = c(-6,6)), aes(x)) +
  stat_function(fun = dist2, n = 101, args = list(prob = 0.5, d=1.414, sd_ratio = 2)) +
  ylab("latent density") +
  xlab(expression(theta)) +
  scale_y_continuous(breaks = NULL)

## ---- results='hide'----------------------------------------------------------
######                                  ######
###### Kernel density estimation method ######
######                                  ######
Mod1 <- IRTest_Poly(initialitem = initialitem,
                    data = data,
                    model = "GPCM",
                    latent_dist = "KDE",
                    bandwidth = "SJ-ste",
                    max_iter = 200,
                    threshold = .001)

######                      ######
###### Normality assumption ######
######                      ######
#  Mod1 <- IRTest_Poly(initialitem = initialitem,
#                      data = data,
#                      model = "GPCM",
#                      latent_dist = "Normal",
#                      max_iter = 200,
#                      threshold = .001)

######                            ######
###### Empirical histogram method ######
######                            ######
#  Mod1 <- IRTest_Poly(initialitem = initialitem,
#                      data = data,
#                      model = "GPCM",
#                      latent_dist = "EHM",
#                      max_iter = 200,
#                      threshold = .001)

######                                             ######
###### Two-component Gaussian mixture distribution ######
######                                             ######
#  Mod1 <- IRTest_Poly(initialitem = initialitem,
#                      data = data,
#                      model = "GPCM",
#                      latent_dist = "Mixture",
#                      max_iter = 200,
#                      threshold = .001)

######                                                        ######
###### Davidian curve (for an arbitrarily chosen case of h=4) ######
######                                                        ######
#  Mod1 <- IRTest_Poly(initialitem = initialitem,
#                      data = data,
#                      model = "GPCM",
#                      latent_dist = "DC",
#                      max_iter = 200,
#                      threshold = .001,
#                      h=4)

## ---- fig.align='center', fig.height=2.5, fig.width=6-------------------------
### The estimated item parameters
Mod1$par_est

### The asymptotic standard errors of item parameters
Mod1$se

### The estimated ability parameters
head(Mod1$theta)

### The estimated latent distribution
plot_LD(Mod1, xlim = c(-6,6))

## -----------------------------------------------------------------------------
Alldata <- DataGeneration(seed = 123456789,
                          model_D = rep(2,5),
                          model_P = "GPCM",
                          categ = rep(3,5),
                          N=1000,
                          nitem_D = 5,
                          nitem_P = 5,
                          d = 1.664,
                          sd_ratio = 1,
                          prob = 0.5)

DataD <- Alldata$data_D
DataP <- Alldata$data_P
itemD <- Alldata$item_D
itemP <- Alldata$item_P
initialitemD <- Alldata$initialitem_D
initialitemP <- Alldata$initialitem_P
theta <- Alldata$theta

## ---- fig.align='center', fig.height=2.5, fig.width=6, echo=F-----------------
ggplot(data = data.frame(x = c(-6,6)), aes(x)) +
  stat_function(fun = dist2, n = 101, args = list(prob = 0.5, d=1.664, sd_ratio = 1)) +
  ylab("latent density") +
  xlab(expression(theta)) +
  scale_y_continuous(breaks = NULL)

## ---- results='hide'----------------------------------------------------------
######                                  ######
###### Kernel density estimation method ######
######                                  ######
Mod1 <- IRTest_Mix(initialitem_D = initialitemD,
                   initialitem_P = initialitemP,
                   data_D = DataD,
                   data_P = DataP,
                   model_D = rep(2,5),
                   model_P = "GPCM",
                   latent_dist = "KDE",
                   bandwidth = "SJ-ste",
                   max_iter = 200,
                   threshold = .001)

######                      ######
###### Normality assumption ######
######                      ######
#  Mod1 <- IRTest_Mix(initialitem_D = initialitemD,
#                     initialitem_P = initialitemP,
#                     data_D = DataD,
#                     data_P = DataP,
#                     model_D = rep(2,5),
#                     model_P = "GPCM",
#                     latent_dist = "Normal",
#                     max_iter = 200,
#                     threshold = .001)

######                            ######
###### Empirical histogram method ######
######                            ######
#  Mod1 <- IRTest_Mix(initialitem_D = initialitemD,
#                     initialitem_P = initialitemP,
#                     data_D = DataD,
#                     data_P = DataP,
#                     model_D = rep(2,5),
#                     model_P = "GPCM",
#                     latent_dist = "EHM",
#                     max_iter = 200,
#                     threshold = .001)

######                                             ######
###### Two-component Gaussian mixture distribution ######
######                                             ######
#  Mod1 <- IRTest_Mix(initialitem_D = initialitemD,
#                     initialitem_P = initialitemP,
#                     data_D = DataD,
#                     data_P = DataP,
#                     model_D = rep(2,5),
#                     model_P = "GPCM",
#                     latent_dist = "Mixture",
#                     max_iter = 200,
#                     threshold = .001)

######                                                        ######
###### Davidian curve (for an arbitrarily chosen case of h=4) ######
######                                                        ######
#  Mod1 <- IRTest_Mix(initialitem_D = initialitemD,
#                     initialitem_P = initialitemP,
#                     data_D = DataD,
#                     data_P = DataP,
#                     model_D = rep(2,5),
#                     model_P = "GPCM",
#                     latent_dist = "DC",
#                     max_iter = 200,
#                     threshold = .001,
#                     h = 4)

## ---- fig.align='center', fig.height=2.5, fig.width=6-------------------------
### The estimated item parameters
Mod1$par_est

### The asymptotic standard errors of item parameters
Mod1$se

### The estimated ability parameters
head(Mod1$theta)

### The estimated latent distribution
plot_LD(Mod1, xlim = c(-6,6))

