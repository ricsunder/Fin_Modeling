library(tidyquant)
library(finstr)
library(XBRL)
library(finstr)

xbrl_url2014 <- "https://www.sec.gov/Archives/edgar/data/320193/000119312514383437/aapl-20140927.xml"

xbrl_url2013 <- 
  "https://www.sec.gov/Archives/edgar/data/320193/000119312513416534/aapl-20130928.xml"

old_o <- options(stringsAsFactors = FALSE)
xbrl_data_aapl2014 <- xbrlDoAll(xbrl_url2014)
xbrl_data_aapl2013 <- xbrlDoAll(xbrl_url2013)
options(old_o)

st2013 <- xbrl_get_statements(xbrl_data_aapl2013)
st2014 <- xbrl_get_statements(xbrl_data_aapl2014)
st2014

balance_sheet2013 <- st2013$StatementOfFinancialPositionClassified
balance_sheet2014 <- st2014$StatementOfFinancialPositionClassified
income2013 <- st2013$StatementOfIncome
income2014 <- st2014$StatementOfIncome
balance_sheet2014

check <- check_statement(balance_sheet2014)
check

check_statement(
  within(balance_sheet2014, InventoryNet <- InventoryNet * 2)
)

check <- check_statement(income2014, element_id = "OperatingIncomeLoss")
check

check$expression[1]

balance_sheet <- merge( balance_sheet2013, balance_sheet2014 )

# merge all statements
st_all <- merge( st2013, st2014 )
# check if balance sheets are merged:
balance_sheet <- st_all$StatementOfFinancialPositionClassified
balance_sheet$endDate

balance_sheet %>% transmute(
  date = endDate, 
  CurrentRatio = AssetsCurrent / LiabilitiesCurrent
)

balance_sheet %>% 
  transmute( Current_Ratio = AssetsCurrent / LiabilitiesCurrent,
             Quick_Ratio =  ( CashAndCashEquivalentsAtCarryingValue + 
                                AvailableForSaleSecuritiesCurrent +
                                AccountsReceivableNetCurrent ) / LiabilitiesCurrent )

merge(balance_sheet, st_all$StatementOfIncome ) %>% 
  calculate( digits = 2,.AccountReceivableLast = lag(AccountsReceivableNetCurrent),
             .AccountReceivableAvg = (.AccountReceivableLast + AccountsReceivableNetCurrent)/2,DaysSalesOutstanding = .AccountReceivableAvg / SalesRevenueNet * 365 )


income2013 %>% 
  calculate(calculations = profit_margins, digits = 2)


