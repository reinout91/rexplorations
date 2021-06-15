library(RSelenium)
library(rvest)

rD <- rsDriver(browser = "chrome", port = 4547L, verbose = FALSE, chromever = "91.0.4472.101" )
remDr <- rD[["client"]]

remDr$navigate("https://shiny-analytics.alliander.local/belastingprognosedashboard")
Sys.sleep(2)
remDr$findElement(using = 'xpath', value = r"{//*[@id="shiny-modal"]/div/div/div[3]/button}")$clickElement()
Sys.sleep(2)
remDr$findElement(using = 'xpath', value = r"{//*[@id="AC5kaart-open_ms_prognose_dashboard"]}")$clickElement()
Sys.sleep(2)
remDr$findElement(using = 'xpath', value = r"{//*[@id="AC5kaart-ms_prognose_dashboard"]/div/div/div[1]/div[1]/div[1]/div/div/div/div[1]}")$clickElement()
Sys.sleep(2)
remDr$findElement(using = 'xpath', value  = r"{//div/div/div/div[2]/div/div[5]}")$clickElement() #selecteer de 5e optie uit de lijst.
Sys.sleep(2)
remDr$findElement(using = 'xpath', value  = r"{/html/body/div[1]/div[2]/div[7]/div/div/div[1]/div[3]/ul/li[7]/a}")$clickElement()
Sys.sleep(2)
remDr$findElement(using = 'xpath', value  = r"{//*[@id="belastingprognose-klantinpassing-inladen_data_installatie"]}")$clickElement()
Sys.sleep(2)

remDr$close()
rm(rD, remDr)
gc()
#system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)

#remDr$findElement(using = 'xpath', value  = r"{//*[@id="DataTables_Table_0_wrapper"]/div[2]/button/span}")$clickElement() #download data


# Sys.sleep(1)
# html <- remDr$getPageSource()[[1]]

# signals <- xml2::read_html(html) %>% ?html_element((css = )
# signals %>% html_children()
# 
# stations <- data.table::data.table(row = onderstation = signals %>% html_children() %>% html_text(), id = signals %>% html_children() %>% html_attr('id'))

