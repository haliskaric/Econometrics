library(quantmod)
getSymbols("AAPL", from="2010-01-03",to="2021-08-31")
AAPL.rtn=diff(log(AAPL$AAPL.Adjusted))  # Compute log returns
chartSeries(AAPL.rtn,theme="white")

getSymbols("DEXUSEU", src="FRED")
head(DEXUSEU)
tail(DEXUSEU)
USEU.rtn=diff(log(DEXUSEU))
chartSeries(DEXUSEU)
chartSeries(USEU.rtn, them="white")
