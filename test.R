install.packages("GGally")
library(readr)
library(GGally)
library(dplyr)
social <- read.csv("digital.csv")
pairs(~ mental_health_score + phone_usage_hours + social_media_hours
      + sleep_quality + caffeine_intake_mg_per_day,
      data=social, pch=20)
social_model <- lm(mental_health_score ~ phone_usage_hours + social_media_hours
      + sleep_quality + caffeine_intake_mg_per_day,
      data=social)
summary(social_model)
plot(social_model, which=1)



all_seasons <- read_csv("NBA_Player_Stats[1].csv")
all_seasons <- all_seasons |>
  filter(Year == "2016-2017" | Year == "2017-2018" | Year == "2018-2019")

f1 <- all_seasons |>
  filter(Player == "Aaron Brooks")
View(f1)
f2 <- nba |>
  filter(Player.x == "Aaron Brooks")
View(f2)

nba_model <- lm(MP ~ G + STL + PF + Age + Year, data=all_seasons)
pairs(formula = ~ MP + G + STL + PF + Age, data=all_seasons, pch=20)

nba_model <- lm(PTS ~ `2PA` + `3PA` + Age + Year, data=all_seasons)
pairs(formula = ~ PTS + `2PA` + `3PA` + Age, data=all_seasons, pch=20)

plot(nba_model, which=1)
plot(nba_model, which=2)

nba <- read.csv("nba_cleaned.csv")
nba <- nba |> mutate(Salary = log(Salary))
nba_model <- lm(Salary ~ G + PTS + PF + AST + Role, data=nba)
summary(nba_model)
plot(nba_model, which=1)
graph_summaries()

# Box-Cox Transform

pairs(~ Salary + PTS + PF + AST + STL, data=nba)
boxplot(nba$Salary~nba$Pos1)
# should see different boxes for each dummy variable, if same, should group them later in part 2
library(MASS)
get_boxcox_response <- function(model) {
  response <- model_response_variables(model) # get the name of the response variable
  n <- nrow(model$model)
  b <- boxcox(model)
  lambda <- b$x[which.max(b$y)]
  geom_mean <- exp(1/n * sum(log(model$model[[response]])))
  boxcox_Y <- geom_mean^(1-lambda) * (model$model[[response]]^lambda - 1)/lambda
  # new_formula <- update(formula(f3), boxcox_Y ~ .)
  # result <- lm(new_formula, data = model$model)
  return(boxcox_Y)
}


# test
nba_salary <- nba |> filter(Salary > 0)
plot(x = nba_salary$PTS, y = nba_salary$Salary)
plot(x = nba_filtered$PTS, y = nba_filtered$Salary)
plot(x = nba_bum$PTS, y = nba_bum$Salary)
nba_salary |> filter(PTS > 20) -> df
nba_salary |> filter(Tm != "BRK" & Tm != "PHO" & Tm != "CHI") -> nba_filtered
nba_salary |> filter(Tm == "BRK" | Tm == "PHO" | Tm == "CHI") -> nba_bum
nba_salary |> filter(Tmr == "CHI") -> nba_bum

# Alternative
pairs(~ GS + PTS + MP + X3PA + Fvot + mean_views, data=nba)
lm( GS ~ PTS + MP + X3PA + Pos1, data=nba) -> dm
plot(dm, which=1)
plot(dm, which=2)
ggplot() +
  geom_point(aes(x = dm$fitted.values, y = nba$GS)) +
  geom_abline(color = "blue")

# Group more Pos1 together
nba <- nba |> mutate(tPos1 = fct_collapse(Pos1, "SG+PF+PG" = c("SG", "PF", "PG")))

# Transformations
nba <- nba |>
  mutate(tSalary = sqrt(Salary), tPTS = log(PTS), tAST = log(AST),
         tPF = log(PF), tTRB = log(TRB)) |>
  filter(is.finite(tPTS), is.finite(tAST), is.finite(tPF), is.finite(tTRB))

# Add Box-Cox
nba <- nba |>
  mutate(bcSalary = get_boxcox_response(f3))

# Partial F tests
nba_model <- lm(Salary ~ PTS + PF + AST + STL + Pos1, data=nba)
f1 <- lm(sqrt(Salary) ~ PTS + PF + AST + STL + Pos1, data=nba)
autoplot(f1, which = c(1, 2))

f1 <- lm(log(Salary) ~ sqrt(PTS) + sqrt(AST) + Pos1, data=nba)
# this one!
f1 <- lm(log(Salary) ~ sqrt(PTS) + I(AST^(1/3)) + log(Fvot) + sqrt(TRB), data=nba)
f2 <- lm(log(Salary) ~ sqrt(PTS) + I(AST^(1/3)) + log(Fvot) + sqrt(TRB) + tPos1, data=nba)
autoplot(f2, which = c(1, 2))
graph_summaries_2(f2)
residualPlots(f2)

pairs(formula(f2), data = nba)
pairs(formula(nba_model), data = nba)

ggplot(f2) +
  geom_point(aes(y = f2$residuals, x = f2$model$`sqrt(TRB)`))

ggplot(f2, aes(y = f2$residuals, x = f2$model$`sqrt(TRB)`)) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ x)

residualPlots(f2)

fvars <- all.vars(formula(f2))
fvars <- c(fvars[-1], fvars[1])
ggpairs(fvars, data = nba)
pairs(sqrt(Salary) ~ sqrt(PTS) + PF + sqrt(AST) + STL ,data = nba)
hist(sqrt(nba$STL))

f1 <- lm(tSalary ~ tPTS + tAST + STL + Pos1, data=nba)
f2 <- lm(tSalary ~ tPTS + Fvot + tAST + STL + Pos1, data=nba)
anova(f1, f2)

f3 <- lm(Salary ~ tPTS + tAST + STL + GS + Pos1, data=nba)
f3_bc <- lm(update(formula(f3), bcSalary ~ .), data = nba)
f4 <- lm(tSalary ~ tPTS + tAST + STL + GS + Pos1, data=nba)
# seems like box-cox doesn't change much compared to log transform
# or
library("ggfortify")
autoplot(f1, which = c(1, 2))
autoplot(f2, which = c(1, 2))
autoplot(f3_bc, which = c(1, 2), size = 2)
autoplot(f4, which = c(1, 2), size = 2)
par(mfrow = c(1, 1))
plot(f3_bc, which = 2)
plot(f4, which = 2)
graph_summaries_2(f4)

plot(hatvalues(f4), type = 'h')

ggplot(f1) +
  geom_point(aes(y = f1$residuals, x = f1$model$tAST))

library("car")
car::residualPlot(f1)

fvars <- all.vars(formula(f4))
fvars <- c(fvars[-1], fvars[1])
ggpairs(fvars, data = nba)
ggpairs(fvars, data = nba,
        diag = list(continuous = wrap("densityDiag")),
        discrete = list(continuous = wrap("facethist", breaks = )))

# VIF
if(!require("car")) install.packages("car")
library("car")

# Leverage points
get_h_bar <- function(model) {
  return(length(model$coefficients) / nrow(model$model))
}
# levs <- which(hatvalues(nba_model) > 2 * h_hat)


# or just
plot(hatvalues(nba_model), type = 'h')
abline(h = 2 * h_hat)
