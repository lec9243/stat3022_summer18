data(Volts)
Volts = transform(Volts, logvolt = log(Voltage))
plot(Volts$Time, Volts$Voltage)
