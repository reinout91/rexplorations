#' @title aki_hackathon_sta
#' @description Season trend analyse van profieldata
#' @import prophet
#' @import data.table
#' @import plotly
aki_hackathon_sta <- function(){
  # profiel_data <- readRDS("/data/workspace/AKI_hackathon_sta/data_alkmaar.RDS")
  profiel_data <- readRDS(system.file('rds/data_alkmaar.RDS', package = packageName()))

  # forecast_data <- unique(forecast_data#[year < 2022, .(ds, y)])
  
  profiel_data$installatie_profiel$DATUM_TIJD <- paste0(profiel_data$installatie_profiel$DATUM_TIJD, ":00")
  
  forecast_data <- profiel_data$installatie_profiel[JAAR %in% c(2020:2025), .(ds = DATUM_TIJD, y = BELASTING)]
  
  model <- prophet(forecast_data)
  future <- make_future_dataframe(model, periods=60*60)
  forecast <- predict(model, future)
  
  ggplotly(plot(model, forecast))
  
  
  prophet_plot_components(model, forecast)
  
  ggplotly(prophet_plot_components(model, forecast)[[1]])
  
  return(1)
}


#' @title aki_hackathon_sta2
#' @description Season trend analyse van profieldata2
#' @import fpp3
#' @import data.table
#' @import slider
#' @import spectral
#' @import control
#' @import plotly
#' @import patchwork
aki_hackathon_sta2 <- function(){
  trucje<- packageName()
  if (is.null(trucje)) {trucje <- 'PROJECTNAME'}
  
  # profiel_data <- readRDS("/data/workspace/AKI_hackathon_sta/data_alkmaar.RDS")
  profiel_data <- readRDS(system.file('rds/data_alkmaar.RDS', package = trucje))
  
  # forecast_data <- unique(forecast_data#[year < 2022, .(ds, y)])
  
  profiel_data$installatie_profiel$DATUM_TIJD <- paste0(profiel_data$installatie_profiel$DATUM_TIJD, ":00")
  forecast_data <- profiel_data$installatie_profiel[JAAR %in% c(2020:2025), .(ds = lubridate::as_datetime(DATUM_TIJD), y = BELASTING)]
  schrikkeldag <-forecast_data[ds < as.Date('2020-02-23') & ds >= as.Date('2020-02-22')][, ds:= ds+ days(7)]
  forecast_data <- rbind(forecast_data, schrikkeldag)
  
  #teken profiel
  series <- tsibble(forecast_data, index=ds)
  series %>%
    autoplot(y, colour="gray") %>% ggplotly()
  # --> zomermaanden kleiner verschil tussen dag/nacht dan in de winter.
  # --> dat maakt het wat lastig te vangen in een puur additief model.
  
  
  #Fourier analyse
  ft <- stats::fft(series$y)
  t_s <- 15 * 60 #[sec] 
  
  freq <- (1:length(ft)) * t_s / length(ft)
  A <- sqrt(Re(ft)^2 + Im(ft)^2) # length of the number
  PSD <- data.frame(freq, A)
  ggplotly(ggplot(PSD, aes(x = freq, y = A)) + geom_line() +
             xlab("frequency rad/s") +
             ylab("spectral density"))
  
  #TODO Dit Power density plot interpreteren.
  
  # Filter ontwerp in R, daarvoor kun je de package control gebruiken.
  # Eventueel zou je ook nog kunnen kijken naar sysid, al betreft dat meer functies voor het interpreteren van step en frequency responses.
  signal <- as.matrix(series$y)
  
  #filter example 1 (simpel low pass filter)
  n_samples <- 100
  t <- seq(0, 1, len = n_samples)                                               # 1 second sample
  t_s <- 1/n_samples
  cutoff <- 10 #hz
  filter_order <- 1
  
  bf <- butter(filter_order, t_s * cutoff, type = "low")                        # 10 Hz low-pass filter
  x <- sin(2 * pi * t * 2.3) + 0.25 * rnorm(length(t))                          # 2.3 Hz sinusoid+noise
  
  
  z <- filter(bf, x) # apply filter
  u <- filtfilt(bf, x)
  ggplotly(qplot(t, x, geom = "line") +
    geom_line(aes(x=t, y=z), col = "red")+
    geom_line(aes(x=t, y=u), col = "green") +
      theme_light())

  
  freqz(bf)
  
  num <- c(1)
  den <- c(0.5, 1)
  filter <- control::tf(num, den) #TODO Deze nog ontwerpen
  bodeplot(tf2ss(num, den))
  
  resp_bode <- bode(filter, seq(0, 100, length = 10000))
  
     
  bode <-  ggplot(data.frame(resp_bode), aes(x = w)) +
    geom_line() + scale_x_log10()
  
  bode_gain <- bode + aes(y = mag) +
    xlab("Frequency [rad/sec]") +
    ylab("Magnitude [dB]") + 
    labs(title = "Bode Magnitude")+
    theme_minimal()
  
  bode_phase <- bode + aes(y = phase) +
    xlab("Frequency [rad/sec]") +
    ylab("Phase [deg]") +
    labs(title = "Bode Phase") + theme_minimal()
  
  bode_gain + bode_phase + plot_layout(ncol = 1)
  
  bodeplot <- plotly::subplot(bode_gain, bode_phase, nrows = 2, shareX = TRUE, shareY = FALSE, titleY = TRUE, titleX = TRUE)
  
  control::lsimplot(sys = filter, u = signal, t = series$ds)
  resp = lsim(sys = filter, u = -signal, t = series$ds)
  mmm <- data.frame(t=resp$t, filtered = resp$y[1,], original = series$y)
  (ggplot(mmm) + 
    geom_line(aes(x=t,y=original), colour = "gray") +
    geom_line(aes(x=t,y=-filtered), colour = "red")) %>% ggplotly
    
  
  #TODO: Dit idee verder uitwerken, filteren per tijdblok, bijvoorbeeld dagen etc.
  #Zelf dus kiezen welke tijdsperiodes nuttig zijn.
  
  #TODO;
  #Arima modellen zijn de gegeneraliseerde vorm voorfrequentiedecompositie.
  #Er bestaat ook SARIMA, Seasonal Arima, waarin specifieke frequentiebereuken voor dagen, weken, maanden etc zijn opgenomen.
  #TODO Test of SARIMA ook nog nuttig kan zijn.
  
  
  #Volledige decompositie
  dcmp <- series %>%
    model(stl = STL(y))
  
  decomp <- components(dcmp)
  
  decomp %>%
    autoplot(decomp$y, colour="gray") +
    geom_line(aes(x=decomp$ds, y=decomp$trend), colour = "#D55E00") +
    labs(
      y = "Vermogen KVA",
      title = "Vermogen KVA"
    )
  
  components(dcmp) %>% autoplot() %>% ggplotly()
  
  #Plotje van het trenddeel van de decompositie
  plt <- components(dcmp) %>%
    as_tsibble() %>%
    autoplot(y, colour="gray") +
    geom_line(aes(y=trend), colour = "#D55E00") +
    labs(
      y = "Vermogen KVA",
      title = "Vermogen KVA - trend"
    ) 
  plt %>% ggplotly
  
  
  #Moving average van 1 dag
  series <- series %>% mutate(m_avg = series %>% {slider::slide_dbl(.$y, mean,
                    .before = 4*12, .after = 4*12, .complete = TRUE)})
  
  plt<- series %>%
    autoplot(y, colour="gray") +
    geom_line(aes(y = `m_avg`), colour = "#D55E00") +
    labs(y = "% of GDP",
         title = "Vermogen KVA - moving average 1 dag") +
    guides(colour = guide_legend(title = "series")) 
  ggplotly(plt)
  #TODO: Stop deze in een high pass filter en bepaal de amplitude.
  

  
  #Prophet
  forecast_data <- profiel_data$installatie_profiel[JAAR %in% c(2020:2025), .(ds = DATUM_TIJD, y = BELASTING)]

  model <- prophet(forecast_data)
  future <- make_future_dataframe(model, periods=1)
  forecast <- predict(model, future)

  ggplotly(plot(model, forecast))


  prophet_plot_components(model, forecast)

  ggplotly(prophet_plot_components(model, forecast)[[1]])
  
  return(1)
}


#' @title aki_hackathon_sta2
#' @description Season trend analyse van profieldata2
#' @import fpp3
#' @import data.table
#' @import slider
#' @import spectral
#' @import control
#' @import plotly
#' @import patchwork
#' @import forecast
#' @import MASS
#' @import tseries
#' 
aki_hackathon_sta3 <- function(){
  
# https://github.com/MGCodesandStats/energy-weather-modelling/blob/master/arima-weather-dublin-airport.R
  library(tsibble)
  
  trucje<- packageName()
  if (is.null(trucje)) {trucje <- 'PROJECTNAME'}
  
  # profiel_data <- readRDS("/data/workspace/AKI_hackathon_sta/data_alkmaar.RDS")
  profiel_data <- readRDS(system.file('rds/data_alkmaar.RDS', package = trucje))
  
  
  ##voorbeeldset:
  mydata<- read.csv("inst/extdata/mly532.csv")
  attach(mydata)
  summary(mydata)
  meant <- ts(mydata$meant[1:732], start = c(1941,11), frequency = 12)
  
  
  ##einde voorbeeld.
  
  
  
  # forecast_data <- unique(forecast_data#[year < 2022, .(ds, y)])
  
  profiel_data$installatie_profiel$DATUM_TIJD <- paste0(profiel_data$installatie_profiel$DATUM_TIJD, ":00")
  forecast_data <- profiel_data$installatie_profiel[JAAR %in% c(2020:2025), .(ds = lubridate::as_datetime(DATUM_TIJD), y = BELASTING)]
  schrikkeldag <-forecast_data[ds < as.Date('2020-02-23') & ds >= as.Date('2020-02-22')][, ds:= ds+ days(7)]
  forecast_data <- rbind(forecast_data, schrikkeldag)
  
  #teken profiel
  series <- tsibble(forecast_data, index=ds)
  series %>%
    autoplot(y, colour="gray") %>% ggplotly()
  
  # ACF, PACF and Dickey-Fuller Test
  acf(meant, lag.max=20)
  pacf(meant, lag.max=20)
  adf.test(meant)
  
  components <- decompose(meant)
  components
  plot(components)
  
  # ARIMA
  fitweatherarima<-auto.arima(meant, trace=TRUE, test="kpss", ic="bic")
  fitweatherarima
  confint(fitweatherarima)
  
  # Forecasted Values From ARIMA
  forecastedvalues=forecast(fitweatherarima,h=183)
  forecastedvalues
  plot(forecastedvalues)
  
  # Test Values
  test=mydata$meant[733:915]
  test
  
  # Ljung-Box
  Box.test(fitweatherarima$resid, lag=4, type="Ljung-Box")
  Box.test(fitweatherarima$resid, lag=8, type="Ljung-Box")
  Box.test(fitweatherarima$resid, lag=12, type="Ljung-Box")
  
  #RMSE
  RMSE <- sqrt(mean((forecastedvalues$mean - test)^2))
  RMSE
  mean(test)

}
