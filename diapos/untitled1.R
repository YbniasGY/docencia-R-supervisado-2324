
library(tidyverse)
library(fastDummies)
iris_tb <- as_tibble(iris)

iris_preproc <- 
  iris |>
  dummy_cols(select_columns = "Species", remove_first_dummy = TRUE) |> 
  as_tibble() |> 
  select(-Species)

# outliers
hist(iris_preproc$Sepal.Width) # --> simétrica
outlier_Sepal.Width <-
  abs(outliers::scores(iris_preproc$Sepal.Width, type = "z")) > 2
mean_no_outliers <-
  mean(iris_preproc$Sepal.Width[!outlier_Sepal.Width])

hist(iris_preproc$Petal.Width)
hist(iris_preproc$Petal.Length)

iris_outliers <- 
  iris_preproc |> 
  mutate(Sepal.Width =
           if_else(outlier_Sepal.Width, mean_no_outliers, Sepal.Width)) |> 
  filter(abs(outliers::scores(iris_preproc$Sepal.Length, type = "z")) < 2)


ajuste_saturado <- lm(data = iris_outliers, formula = Sepal.Length ~ .)
ajuste_saturado |> summary()
performance::check_collinearity(ajuste_saturado)
iris_preproc |> corrr::correlate()

iris_preproc <- 
  iris_outliers |> 
  select(-Species_versicolor)
ajuste_saturado <- lm(data = iris_preproc, formula = Sepal.Length ~ .)
ajuste_saturado |> summary()
performance::check_collinearity(ajuste_saturado)
iris_preproc |> corrr::correlate()

iris_preproc <- 
  iris_preproc |> 
  select(-Sepal.Width)
ajuste_saturado <- lm(data = iris_preproc, formula = Sepal.Length ~ .)
ajuste_saturado |> summary()
performance::check_collinearity(ajuste_saturado)
iris_preproc |> corrr::correlate()

# DIAGNOSIS
performance::check_model(ajuste_saturado)


# 1. LINEALIDAD
# scatter plot
ggplot(tibble("fitted" = ajuste_saturado$fitted.values,
              "residuals" = ajuste_saturado$residuals),
       aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.8) +
  geom_smooth(method = "lm") +
  theme_minimal()
performance::check_outliers(ajuste_saturado)

# residuals vs fitted
lm(data = tibble("fitted" = ajuste_saturado$fitted.values,
                 "residuals" = ajuste_saturado$residuals),
   formula = residuals ~ fitted) |>
  summary()
lm(data = tibble("fitted" = ajuste_saturado$fitted.values,
                 "residuals" = ajuste_saturado$residuals),
   formula = residuals ~ fitted + I(fitted^2)) |>
  summary()
lm(data = tibble("fitted" = ajuste_saturado$fitted.values,
                 "residuals" = ajuste_saturado$residuals),
   formula = residuals ~ fitted + I(fitted^2) + I(fitted^3)) |>
  summary()

termplot(ajuste_saturado, partial.resid = TRUE)

# 2. HETEROCEDASTICITY
performance::check_heteroscedasticity(ajuste_saturado)
ggplot(tibble("id" = 1:length(ajuste_saturado$residuals),
              "residuals" = ajuste_saturado$residuals),
       aes(x = id, y = residuals))+
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal()

lindia::gg_scalelocation(ajuste_saturado, method = "lm")

# 3. NORMALIDAD
performance::check_normality(ajuste_saturado)
ggplot(tibble("residuals" = ajuste_saturado$residuals)) +
  geom_qq(aes(sample = residuals)) +
  geom_qq_line(aes(sample = residuals))

# 4. CORRELADOS
acf(ajuste_saturado$residuals)
performance::check_autocorrelation(ajuste_saturado)

# SELECCIÓN MODELOS
MASS::stepAIC(ajuste_saturado, k = log(nrow(iris_preproc)))
ajuste_BIC <- lm(formula = Sepal.Length ~ Petal.Length, data = iris_preproc)
performance::compare_performance(ajuste_saturado, ajuste_BIC)

# REPETIR CON PETAL WIDTH
# cpu log2

# normality

# Predictors
n <- 200
set.seed(121938)
X1 <- rexp(n, rate = 1 / 5) # Non-negative
X2 <- rchisq(n, df = 5) - 3 # Real

# Response of a linear model
epsilon <- rchisq(n, df = 10) - 10 # Centered error, but not normal
Y <- 10 - 0.5 * X1 + X2 + epsilon

# Transformation of non-normal data to achieve normal-like data (no model)

# Optimal lambda for Box-Cox
BC <- car::powerTransform(lm(X1 ~ 1), family = "bcPower") # Maximum-likelihood fit
# Note we use a regression model with no predictors
(lambdaBC <- BC$lambda) # The optimal lambda
##        Y1 
## 0.2412419
# lambda < 1, so positive skewness is corrected

# Box-Cox transformation
X1Transf <- car::bcPower(U = X1, lambda = lambdaBC)

# Comparison
par(mfrow = c(1, 2))
hist(X1, freq = FALSE, breaks = 10, ylim = c(0, 0.3))
hist(X1Transf, freq = FALSE, breaks = 10, ylim = c(0, 0.3))
