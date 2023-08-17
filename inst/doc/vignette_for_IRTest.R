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
                          latent_dist = "2NM",
                          d = 1.664,
                          sd_ratio = 2,
                          prob = 0.3)

data <- Alldata$data_D
item <- Alldata$item_D
theta <- Alldata$theta
data[1:500, 1] <- NA
data[501:1000, 2] <- NA

## ---- fig.align='center', fig.height=2.5, fig.width=6, echo=F-----------------
ggplot(data = data.frame(x = c(-6,6)), aes(x)) +
  stat_function(fun = dist2, n = 101, args = list(prob = 0.3, d=1.664, sd_ratio = 2)) +
  ylab("latent density") +
  xlab(expression(theta)) +
  scale_y_continuous(breaks = NULL)

## ---- results='hide', message=FALSE-------------------------------------------
Mod1 <- IRTest_Dich(data = data,
                    model = 2,
                    latent_dist = "LLS",
                    h=4)

## ---- fig.align='center', fig.height=6, fig.width=6---------------------------
### Summary
summary(Mod1)

### The estimated item parameters
Mod1$par_est

### The asymptotic standard errors of item parameters
Mod1$se

### The estimated ability parameters
plot(theta, Mod1$theta)
abline(b=1, a=0)

## ---- fig.align='center', fig.height=2.5, fig.width=6-------------------------
### The estimated latent distribution
plot(Mod1) +
  lims(y = c(0, .5))

## -----------------------------------------------------------------------------
item_fit(Mod1)

## -----------------------------------------------------------------------------
reliability(Mod1)

## -----------------------------------------------------------------------------
Alldata <- DataGeneration(seed = 123456789,
                          model_P = "GPCM",
                          categ = rep(c(3,7), each = 5),
                          N=1000,
                          nitem_D = 0,
                          nitem_P = 10,
                          latent_dist = "2NM",
                          d = 1.414,
                          sd_ratio = 2,
                          prob = 0.5)

data <- Alldata$data_P
item <- Alldata$item_P
theta <- Alldata$theta
data[1:500, 1:3] <- NA
data[501:1000, 4:6] <- NA

## ---- fig.align='center', fig.height=2.5, fig.width=6, echo=F-----------------
ggplot(data = data.frame(x = c(-6,6)), aes(x)) +
  stat_function(fun = dist2, n = 101, args = list(prob = 0.5, d=1.414, sd_ratio = 2)) +
  ylab("latent density") +
  xlab(expression(theta)) +
  scale_y_continuous(breaks = NULL)

## ---- results='hide', message=FALSE-------------------------------------------
Mod1 <- IRTest_Poly(data = data,
                    model = "GPCM",
                    latent_dist = "KDE")

## ---- fig.align='center', fig.height=6, fig.width=6---------------------------
### Summary
summary(Mod1)

### The estimated item parameters
Mod1$par_est

### The asymptotic standard errors of item parameters
Mod1$se

### The estimated ability parameters
plot(theta, Mod1$theta)
abline(b=1, a=0)

## ---- fig.align='center', fig.height=2.5, fig.width=6-------------------------
### The estimated latent distribution
plot(Mod1) +
  lims(y = c(0, .5))

## -----------------------------------------------------------------------------
item_fit(Mod1)

## -----------------------------------------------------------------------------
reliability(Mod1)

## -----------------------------------------------------------------------------
Alldata <- DataGeneration(seed = 12345678,
                          model_D = rep(2,5),
                          model_P = "GPCM",
                          categ = rep(5,5),
                          N=1000,
                          nitem_D = 5,
                          nitem_P = 5,
                          latent_dist = "2NM",
                          d = 1.664,
                          sd_ratio = 1,
                          prob = 0.5)

DataD <- Alldata$data_D
DataP <- Alldata$data_P
itemD <- Alldata$item_D
itemP <- Alldata$item_P
theta <- Alldata$theta

DataD[1:250, 1] <- NA
DataD[251:500, 2] <- NA
DataP[501:750, 1] <- NA
DataP[751:1000, 2] <- NA

## ---- fig.align='center', fig.height=2.5, fig.width=6, echo=F-----------------
ggplot() +
  stat_function(fun = dist2, n = 101, args = list(prob = 0.5, d=1.664, sd_ratio = 1)) +
  lims(x = c(-6,6), y = c(0, .5)) +
  ylab("latent density") +
  xlab(expression(theta)) +
  scale_y_continuous(breaks = NULL)

## ---- results='hide', message=FALSE-------------------------------------------
Mod1 <- IRTest_Mix(data_D = DataD,
                   data_P = DataP,
                   model_D = "2PL",
                   model_P = "GPCM",
                   latent_dist = "KDE")

## ---- fig.align='center', fig.height=6, fig.width=6---------------------------
### Summary
summary(Mod1)

### The estimated item parameters
Mod1$par_est

### The asymptotic standard errors of item parameters
Mod1$se

### The estimated ability parameters
plot(theta, Mod1$theta)
abline(b=1, a=0)

## ---- fig.align='center', fig.height=2.5, fig.width=6-------------------------
### The estimated latent distribution
plot(Mod1) +
  lims(y = c(0, .5))

## -----------------------------------------------------------------------------
item_fit(Mod1)

## -----------------------------------------------------------------------------
reliability(Mod1)

