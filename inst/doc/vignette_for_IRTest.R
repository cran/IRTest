## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(IRTest)
library(ggplot2)
library(gridExtra)

## -----------------------------------------------------------------------------
Alldata <- DataGeneration(seed = 123456789,
                          model_D = rep(2, each=15),
                          N=1000,
                          nitem_D = 15,
                          nitem_P = 0,
                          latent_dist = "2NM",
                          d = 1.664,
                          sd_ratio = 2,
                          prob = 0.3)

data <- Alldata$data_D
theta <- Alldata$theta
data[1:500, 1] <- NA
data[501:1000, 2] <- NA
colnames(data) <- paste0("item", 1:15)

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

## ---- fig.align='center', fig.asp=.6, fig.width=6-----------------------------
plot(Mod1, mapping = aes(colour="Estimated"), linewidth = 1) +
  lims(
    y = c(0, .75)
  )+
  geom_line(
    mapping=aes(
      x=seq(-6,6,length=121), 
      y=dist2(
        seq(-6,6,length=121),
        prob = .3, 
        d=1.664, 
        sd_ratio = 2
        ), 
      colour="True"),
    linewidth = 1)+
  labs(
    title="The estimated latent density using '2NM'", colour= "Type"
    )+
  theme_bw()

## ---- fig.align='center', fig.asp=0.8, fig.width=6----------------------------
p1 <- plot_item(Mod1,1)
p2 <- plot_item(Mod1,4)
p3 <- plot_item(Mod1,8)
p4 <- plot_item(Mod1,10)
grid.arrange(p1, p2, p3, p4, ncol=2, nrow=2)

## -----------------------------------------------------------------------------
item_fit(Mod1)

## -----------------------------------------------------------------------------
reliability(Mod1)

## ---- fig.align='center', fig.asp=.7, fig.width=6-----------------------------
set.seed(1)
selected_examinees <- sample(1:1000,6)
post_sample <- 
  data.frame(
    X = rep(seq(-6,6, length.out=121),6), 
    prior = rep(Mod1$Ak/(Mod1$quad[2]-Mod1$quad[1]), 6),
    posterior = 10*c(t(Mod1$Pk[selected_examinees,])), 
    ID = rep(paste("examinee", selected_examinees), each=121)
    )

ggplot(data=post_sample, mapping=aes(x=X))+
  geom_line(mapping=aes(y=posterior, group=ID, color='Posterior'))+
  geom_line(mapping=aes(y=prior, group=ID, color='Prior'))+
  labs(title="Posterior densities for selected examinees", x=expression(theta), y='density')+
  facet_wrap(~ID, ncol=2)+
  theme_bw()

## ---- fig.align='center', fig.asp=.8, fig.width=6-----------------------------
ggplot()+
  stat_function(
    fun = inform_f_test,
    args = list(Mod1)
  )+ 
  stat_function(
    fun=inform_f_item,
    args = list(Mod1, 1),
    mapping = aes(color="Item 1")
  )+
  stat_function(
    fun=inform_f_item,
    args = list(Mod1, 2),
    mapping = aes(color="Item 2")
  )+
  stat_function(
    fun=inform_f_item,
    args = list(Mod1, 3),
    mapping = aes(color="Item 3")
  )+
  stat_function(
    fun=inform_f_item,
    args = list(Mod1, 4),
    mapping = aes(color="Item 4")
  )+
  stat_function(
    fun=inform_f_item,
    args = list(Mod1, 5),
    mapping = aes(color="Item 5")
  )+
  lims(x=c(-6,6))+
  labs(title="Test information function", x=expression(theta), y='information')+
  theme_bw()

## -----------------------------------------------------------------------------
Alldata <- DataGeneration(seed = 123456789,
                          model_P = "GRM",
                          categ = rep(c(3,7), each = 7),
                          N=1000,
                          nitem_D = 0,
                          nitem_P = 14,
                          latent_dist = "2NM",
                          d = 1.664,
                          sd_ratio = 2,
                          prob = 0.3)

data <- Alldata$data_P
theta <- Alldata$theta
data[1:500, 1:3] <- NA
data[501:1000, 4:6] <- NA
colnames(data) <- paste0("item", 1:14)

## ---- results='hide', message=FALSE-------------------------------------------
Mod1 <- IRTest_Poly(data = data,
                    model = "GRM",
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

## ---- fig.align='center', fig.asp=.6, fig.width=6-----------------------------
plot(Mod1, mapping = aes(colour="Estimated"), linewidth = 1) +
  lims(
    y = c(0, .75)
  )+
  geom_line(
    mapping=aes(
      x=seq(-6,6,length=121), 
      y=dist2(
        seq(-6,6,length=121),
        prob = .3, 
        d=1.664, 
        sd_ratio = 2
        ), 
      colour="True"),
    linewidth = 1)+
  labs(
    title="The estimated latent density using '2NM'", colour= "Type"
    )+
  theme_bw()

## ---- fig.align='center', fig.asp=0.8, fig.width=6----------------------------
p1 <- plot_item(Mod1,1)
p2 <- plot_item(Mod1,4)
p3 <- plot_item(Mod1,8)
p4 <- plot_item(Mod1,10)
grid.arrange(p1, p2, p3, p4, ncol=2, nrow=2)

## -----------------------------------------------------------------------------
item_fit(Mod1)

## -----------------------------------------------------------------------------
reliability(Mod1)

## ---- fig.align='center', fig.asp=.7, fig.width=6-----------------------------
set.seed(1)
selected_examinees <- sample(1:1000,6)
post_sample <- 
  data.frame(
    X = rep(seq(-6,6, length.out=121),6), 
    prior = rep(Mod1$Ak/(Mod1$quad[2]-Mod1$quad[1]), 6),
    posterior = 10*c(t(Mod1$Pk[selected_examinees,])), 
    ID = rep(paste("examinee", selected_examinees), each=121)
    )

ggplot(data=post_sample, mapping=aes(x=X))+
  geom_line(mapping=aes(y=posterior, group=ID, color='Posterior'))+
  geom_line(mapping=aes(y=prior, group=ID, color='Prior'))+
  labs(title="Posterior densities for selected examinees", x=expression(theta), y='density')+
  facet_wrap(~ID, ncol=2)+
  theme_bw()

## ---- fig.align='center', fig.asp=.8, fig.width=6-----------------------------
ggplot()+
  stat_function(
    fun = inform_f_test,
    args = list(Mod1)
  )+ 
  stat_function(
    fun=inform_f_item,
    args = list(Mod1, 1),
    mapping = aes(color="Item 1 (3 cats)")
  )+
  stat_function(
    fun=inform_f_item,
    args = list(Mod1, 2),
    mapping = aes(color="Item 2 (3 cats)")
  )+
  stat_function(
    fun=inform_f_item,
    args = list(Mod1, 3),
    mapping = aes(color="Item 3 (3 cats)")
  )+
  stat_function(
    fun=inform_f_item,
    args = list(Mod1, 8),
    mapping = aes(color="Item 8 (7 cats)")
  )+
  stat_function(
    fun=inform_f_item,
    args = list(Mod1, 9),
    mapping = aes(color="Item 9 (7 cats)")
  )+
  stat_function(
    fun=inform_f_item,
    args = list(Mod1, 10, "p"),
    mapping = aes(color="Item10 (7 cats)")
  )+
  lims(x=c(-6,6))+
  labs(title="Test information function", x=expression(theta), y='information')+
  theme_bw()

## -----------------------------------------------------------------------------
Alldata <- DataGeneration(seed = 123456789,
                          model_D = rep(2,10),
                          model_P = "GRM",
                          categ = rep(5,5),
                          N=1000,
                          nitem_D = 10,
                          nitem_P = 5,
                          latent_dist = "2NM",
                          d = 1.664,
                          sd_ratio = 1,
                          prob = 0.5)

DataD <- Alldata$data_D
DataP <- Alldata$data_P
theta <- Alldata$theta

DataD[1:250, 1] <- NA
DataD[251:500, 2] <- NA
DataP[501:750, 1] <- NA
DataP[751:1000, 2] <- NA
colnames(DataD) <- paste0("item", 1:10)
colnames(DataP) <- paste0("item", 1:5)

## ---- results='hide', message=FALSE-------------------------------------------
Mod1 <- IRTest_Mix(data_D = DataD,
                   data_P = DataP,
                   model_D = "2PL",
                   model_P = "GRM",
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

## ---- fig.align='center', fig.asp=.6, fig.width=6-----------------------------
plot(Mod1, mapping = aes(colour="Estimated"), linewidth = 1) +
  lims(
    y = c(0, .75)
  )+
  geom_line(
    mapping=aes(
      x=seq(-6,6,length=121), 
      y=dist2(
        seq(-6,6,length=121),
        prob = .3, 
        d=1.664, 
        sd_ratio = 2
        ), 
      colour="True"),
    linewidth = 1)+
  labs(
    title="The estimated latent density using '2NM'", colour= "Type"
    )+
  theme_bw()

## ---- fig.align='center', fig.asp=0.8, fig.width=6----------------------------
p1 <- plot_item(Mod1,1, type="d")
p2 <- plot_item(Mod1,4, type="d")
p3 <- plot_item(Mod1,8, type="d")
p4 <- plot_item(Mod1,10, type="d")
grid.arrange(p1, p2, p3, p4, ncol=2, nrow=2)

## ---- fig.align='center', fig.asp=0.8, fig.width=6----------------------------
p1 <- plot_item(Mod1,1, type="p")
p2 <- plot_item(Mod1,2, type="p")
p3 <- plot_item(Mod1,3, type="p")
p4 <- plot_item(Mod1,4, type="p")
grid.arrange(p1, p2, p3, p4, ncol=2, nrow=2)

## -----------------------------------------------------------------------------
item_fit(Mod1)

## -----------------------------------------------------------------------------
reliability(Mod1)

## ---- fig.align='center', fig.asp=.7, fig.width=6-----------------------------
set.seed(1)
selected_examinees <- sample(1:1000,6)
post_sample <- 
  data.frame(
    X = rep(seq(-6,6, length.out=121),6), 
    prior = rep(Mod1$Ak/(Mod1$quad[2]-Mod1$quad[1]), 6),
    posterior = 10*c(t(Mod1$Pk[selected_examinees,])), 
    ID = rep(paste("examinee", selected_examinees), each=121)
    )

ggplot(data=post_sample, mapping=aes(x=X))+
  geom_line(mapping=aes(y=posterior, group=ID, color='Posterior'))+
  geom_line(mapping=aes(y=prior, group=ID, color='Prior'))+
  labs(title="Posterior densities for selected examinees", x=expression(theta), y='density')+
  facet_wrap(~ID, ncol=2)+
  theme_bw()

## ---- fig.align='center', fig.asp=.8, fig.width=8-----------------------------
ggplot()+
  stat_function(
    fun = inform_f_test,
    args = list(Mod1)
  )+ 
  stat_function(
    fun=inform_f_item,
    args = list(Mod1, 1, "d"),
    mapping = aes(color="Dichotomous Item 1")
  )+
  stat_function(
    fun=inform_f_item,
    args = list(Mod1, 2, "d"),
    mapping = aes(color="Dichotomous Item 2")
  )+
  stat_function(
    fun=inform_f_item,
    args = list(Mod1, 3, "d"),
    mapping = aes(color="Dichotomous Item 3")
  )+
  stat_function(
    fun=inform_f_item,
    args = list(Mod1, 1, "p"),
    mapping = aes(color="Polytomous Item 1")
  )+
  stat_function(
    fun=inform_f_item,
    args = list(Mod1, 2, "p"),
    mapping = aes(color="Polytomous Item 2")
  )+
  stat_function(
    fun=inform_f_item,
    args = list(Mod1, 3, "p"),
    mapping = aes(color="Polytomous Item 3")
  )+
  lims(x=c(-6,6))+
  labs(title="Test information function", x=expression(theta), y='information')+
  theme_bw()

