library(WDI)

data = WDI(country = "GBR", indicator = "NY.GDP.MKTP.KD", start = 1960, end = 2020, extra = FALSE)
lagpad <- function(x, k) {
  if (k>0) {
    return (c(rep(NA, k), x)[1 : length(x)] );
  }
  else {
    return (c(x[(-k+1) : length(x)], rep(NA, -k)));
  }
}

data$lgdp = log(data$NY.GDP.MKTP.KD)
Trend = lm(data$lgdp ~ data$year)
data$residuals = as.vector(resid(Trend))
data$lresiduals = lagpad(data$residuals, k = 1)

tech_reg = lm(data$residuals ~ data$lresiduals)
summary(tech_reg)