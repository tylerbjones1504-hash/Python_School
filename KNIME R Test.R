# Verify
R.home()
library(Rserve)
packageVersion("Rserve")

# Install
install.packages("Rserve")
library(Rserve)
R.home()

# Install
install.packages("Cairo")
library(Cairo)

# Code Snippet 1
print(names(knime.in))
print(head(knime.in))
print(str(knime.in))

knime.out <- knime.in


# Code Snippet 2
df <- knime.in
time_raw <- as.character(df[["row ID"]])
y <- as.numeric(df[["cluster_26"]])
time_parsed <- as.POSIXct(time_raw, format = "%Y-%m-%dT%H:%M", tz = "UTC")
print(head(time_parsed))
print(length(y))
print(sum(is.na(y)))
fit <- arima(y, order = c(1,1,1))
print(fit)
knime.out <- df