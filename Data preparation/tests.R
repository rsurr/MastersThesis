data("Fishing", package = "mlogit")
library("zoo")
Fish <- mlogit.data(Fishing, varying = c(2:9), shape = "wide", choice = "mode")
m <- mlogit(mode ~ price | income | catch, data = Fish)
# compute a data.frame containing the mean value of the covariates in
# the sample
z <- with(Fish, data.frame(price = tapply(price, idx(m, 2), mean),
                           catch = tapply(catch, idx(m, 2), mean),
                           income = mean(income)))
# compute the marginal effects (the second one is an elasticity
## IGNORE_RDIFF_BEGIN
effects(m, covariate = "income", data = z)
## IGNORE_RDIFF_END
effects(m, covariate = "price", type = "rr", data = z)
effects(m, covariate = "catch", type = "ar", data = z)


################


mlogit1dta %>% 
  group_by(IMAE) %>% 
  summarise(mean=mean(quality, na.rm = T))

mlogit1dta %>% 
  group_by(choice) %>% 
  summarise(mean=mean(quality, na.rm = T))

TEST <- mlogit1dta %>% 
  group_by(CAPACNUM) %>% 
  summarise(mean=mean(quality, na.rm = T)) %>% 
  mutate(na=if_else(is.na(mean), 1, 0))

summary(TEST$na)

mlogit1dta %>% 
  group_by(inst) %>% 
  summarise(mean=mean(quality, na.rm = T))

mlogit1dta %>% 
  group_by(medimae) %>% 
  summarise(mean=mean(quality, na.rm = T))