setwd("/Users/annaisgangolf/Documents/markets/tips/analysis")  

library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tools)
library(xml2)
library(purrr)
library(stats)
library(tidyr)

# ----------------------- Clean TIPS ETF Data ------------------------ #
#
#     This code takes raw data from the respective ETF manager website 
#   and processes it into a usable form. The data is from approximately 
#   2012 to September 2025. The ETF data in this script is TIP, STIP, 
#   TIPZ, STPZ, LTPZ, TIPX, and SPIP. The links to find the respective
#   data are listed in the xx file.  
#     Each ETF has two datasets: the first has volume, closing price, 
#   and the difference between the high price and low price of that day. 
#   The second has AUM, NAV, and shares outstanding.
#     Price data files are sourced from Yahoo Finance and have the same 
#   format for each ETF. NAV datasets are sourced from issuers and thus 
#   may be different.
#     The Price and NAV data are then merged for each ETF. The Price-NAV 
#   datasets are then appended together to create a master dataset with all 
#   baseline data for all ETFs.
#     Finally, premium/discount, price-nav, price-NAV scaled to by AUM, 
#   NAV return, price return, fund flows, and fund flows scaled to AUM
#   are calculated and thus in the final analysis dataset. See 
#   xxx file for formulas for these calculations. 
#
# -------------------- TIP -------------------- #
# Price data
price_tip <- read_excel("data/input/tip/price_tip.xlsx", sheet = "cleandata") %>%
  rename(date = "Exchange Date", price = "Close", volume = "Volume") %>%
  mutate(
    high_low = High - Low
  ) %>%
  select(date, price, volume, high_low) 

  write.csv(price_tip, "data/intermediate/tip/price_tip.csv")

# NAV data
nav_tip <- read_excel("data/input/tip/nav_tip.xlsx", sheet = "Historical") %>%
  rename(share_outstanding = "Shares Outstanding", nav = "NAV per Share", date_str = "As Of") %>%
  mutate(
    share_outstanding = as.numeric(share_outstanding),
    date = as.Date(date_str, format = "%B %d, %Y"),
    aum = share_outstanding * nav
    ) %>%
  select(date, nav, share_outstanding, aum)

  write.csv(nav_tip, "data/intermediate/tip/nav_tip.csv")

# Merge TIP price and NAV
  tip <- full_join(nav_tip, price_tip, by = "date") 
  tip <- tip %>% mutate(ticker = "TIP")

  write.csv(tip, "data/intermediate/tip/metrics_tip.csv")

# -------------------- STIP -------------------- #
  price_stip <- read_excel("data/input/stip/price_stip.xlsx", sheet = "cleandata") %>%
    rename(date = "Exchange Date", price = "Close", volume = "Volume") %>%
    mutate(
      high_low = High - Low
    ) %>%
    select(date, price, volume, high_low) 

  write.csv(price_stip, "data/intermediate/stip/price_stip.csv")
  
# NAV data
  nav_stip <- read_excel("data/input/stip/nav_stip.xlsx", sheet = "Historical") %>%
    rename(share_outstanding = "Shares Outstanding", nav = "NAV per Share", date_str = "As Of") %>%
    mutate(
      share_outstanding = as.numeric(share_outstanding),
      date = as.Date(date_str, format = "%B %d, %Y"),
      aum = share_outstanding * nav
    ) %>%
    select(date, nav, share_outstanding, aum)
  
  write.csv(nav_tip, "data/intermediate/stip/nav_stip.csv")
  
  # Merge TIP price and NAV
  stip <- full_join(nav_stip, price_stip, by = "date")
  stip <- stip %>% mutate(ticker = "STIP")
  
  write.csv(tip, "data/intermediate/stip/metrics_stip.csv")
  
# -------------------- LTPZ -------------------- #
  price_ltpz <- read_excel("data/input/ltpz/price_ltpz.xlsx", sheet = "cleandata") %>%
    rename(date = "Exchange Date", price = "Close", volume = "Volume") %>%
    mutate(
      high_low = High - Low
    ) %>%
    select(date, price, volume, high_low) 

  write.csv(price_ltpz, "data/intermediate/ltpz/price_ltpz.csv")
  
  # NAV data
  nav_ltpz <- read_excel("data/input/ltpz/nav_ltpz.xlsx", sheet = "Historic Levels") %>%
    rename(price = "ETF price per share", nav = "ETF NAV", aum = "ETF AUM", date_str = "Date") %>%
    mutate(
      date = as.Date(date_str, format = "%m/%d/%Y"),
      date = as.Date(date),
      share_outstanding = aum / price
    ) %>%
    select(date, nav, share_outstanding, aum)
  
  write.csv(nav_ltpz, "data/intermediate/ltpz/nav_ltpz.csv")
  
  ltpz <- full_join(nav_ltpz, price_ltpz, by = "date")
  ltpz <- ltpz %>% mutate(ticker = "LTPZ")
  
  write.csv(ltpz, "data/intermediate/ltpz/metrics_ltpz.csv")

# -------------------- TIPZ -------------------- #
  price_tipz <- read.csv("data/input/tipz/tipz_price_vol.csv")  %>%
    rename(price = "open") %>%
    mutate(
      high_low = high - low,
      date = as.Date(date)
    ) %>%
    select(date, price, volume, high_low) 
  write.csv(price_tipz, "data/intermediate/tipz/price_tipz.csv")
  
  # NAV data
  nav_tipz <- read_excel("data/input/tipz/PIMCO TIPZ B3 Exchange Performance.xlsx", sheet = "Historic Levels") %>%
    rename(price = "ETF price per share", nav = "ETF NAV", aum = "ETF AUM", date_str = "Date") %>%
    mutate(
      date = as.Date(date_str, format = "%m/%d/%Y"),
      date = as.Date(date),
      share_outstanding = aum / price
    ) %>%
    select(date, nav, share_outstanding, aum)
  
  write.csv(nav_tipz, "data/intermediate/tipz/nav_tipz.csv")
  
  tipz <- full_join(nav_tipz, price_tipz, by = "date")
  tipz <- tipz %>% mutate(ticker = "TIPZ")
  
  write.csv(tipz, "data/intermediate/tipz/metrics_tipz.csv")
  
# -------------------- STPZ -------------------- #
  price_stpz <- read.csv("data/input/stpz/stpz_price_vol.csv")  %>%
    rename(price = "open") %>%
    mutate(
      high_low = high - low,
      date = as.Date(date)
    ) %>%
    select(date, price, volume, high_low) 
  write.csv(price_stpz, "data/intermediate/stpz/price_stpz.csv")
  
  # NAV data
  nav_stpz <- read_excel("data/input/stpz/PIMCO STPZ B3 Exchange Performance.xlsx", sheet = "Historic Levels") %>%
    rename(price = "ETF price per share", nav = "ETF NAV", aum = "ETF AUM", date_str = "Date") %>%
    mutate(
      date = as.Date(date_str, format = "%m/%d/%Y"),
      date = as.Date(date),
      share_outstanding = aum / price
    ) %>%
    select(date, nav, share_outstanding, aum)
  
  write.csv(nav_stpz, "data/intermediate/stpz/nav_stpz.csv")
  
  stpz <- full_join(nav_stpz, price_stpz, by = "date")
  stpz <- stpz %>% mutate(ticker = "STPZ")
  
  write.csv(stpz, "data/intermediate/stpz/metrics_stpz.csv")
  
# -------------------- TIPX -------------------- #
  price_tipx <- read.csv("data/input/tipx/tipx_price_vol.csv")  %>%
    rename(price = "open") %>%
    mutate(
      high_low = high - low,
      date = as.Date(date)
    ) %>%
    select(date, price, volume, high_low) 
  write.csv(price_tipx, "data/intermediate/tipx/price_tipx.csv")
  
  # NAV data
  nav_tipx <- read_excel("data/input/tipx/navhist-us-en-tipx.xlsx", skip=3) 
  nav_tipx <- nav_tipx[, -c(4, 5, 6)]
  nav_tipx <- nav_tipx[1:3098, ]
  nav_tipx <- nav_tipx %>% rename(date_str = "Date", nav = "NAV", share_outstanding = "Shares Outstanding") %>%
    mutate(
      date_str = as.character(date_str),
      share_outstanding = as.numeric(share_outstanding),
      date = as.Date(date_str, format = "%d-%b-%Y"),
      date = as.Date(date),
      aum = share_outstanding * nav) %>%
    select(date, nav, share_outstanding, aum)

  write.csv(nav_tipx, "data/intermediate/tipx/nav_tipx.csv")
  
  tipx <- full_join(nav_tipx, price_tipx, by = "date")
  tipx <- tipx %>% mutate(ticker = "TIPX")
  
  write.csv(tipx, "data/intermediate/tipx/metrics_tipx.csv")
  
# -------------------- WIP -------------------- #
  price_wip <- read.csv("data/input/wip/wip_price_vol.csv")  %>%
    rename(price = "open") %>%
    mutate(
      high_low = high - low,
      date = as.Date(date)
    ) %>%
    select(date, price, volume, high_low) 
  write.csv(price_wip, "data/intermediate/wip/price_wip.csv")
  
  # NAV data
  nav_wip <- read_excel("data/input/wip/navhist-us-en-wip.xlsx", skip=3) 
  nav_wip <- nav_wip[, -c(4, 5, 6)]
  nav_wip <- nav_wip[1:4411, ]
  nav_wip <- nav_wip %>% rename(date_str = "Date", nav = "NAV", share_outstanding = "Shares Outstanding") %>%
    mutate(
      date_str = as.character(date_str),
      share_outstanding = as.numeric(share_outstanding),
      date = as.Date(date_str, format = "%d-%b-%Y"),
      date = as.Date(date),
      aum = share_outstanding * nav) %>%
    select(date, nav, share_outstanding, aum)
  
  write.csv(nav_wip, "data/intermediate/wip/nav_wip.csv")
  
  wip <- full_join(nav_wip, price_wip, by = "date")
  wip <- wip %>% mutate(ticker = "WIP")
  
  write.csv(wip, "data/intermediate/wip/metrics_wip.csv")

# -------------------- SPIP -------------------- #
  price_spip <- read.csv("data/input/spip/spip_price_vol.csv")  %>%
    rename(price = "open") %>%
    mutate(
      high_low = high - low,
      date = as.Date(date)
    ) %>%
    select(date, price, volume, high_low) 
  write.csv(price_spip, "data/intermediate/spip/price_spip.csv")
  
  # NAV data
  nav_spip <- read_excel("data/input/spip/navhist-us-en-spip.xlsx", skip=3) 
  nav_spip <- nav_spip[, -c(4, 5, 6)]
  nav_spip <- nav_spip[1:4612, ]
  nav_spip <- nav_spip %>% rename(date_str = "Date", nav = "NAV", share_outstanding = "Shares Outstanding") %>%
    mutate(
      date_str = as.character(date_str),
      share_outstanding = as.numeric(share_outstanding),
      date = as.Date(date_str, format = "%d-%b-%Y"),
      date = as.Date(date),
      aum = share_outstanding * nav) %>%
    select(date, nav, share_outstanding, aum)
  
  write.csv(nav_spip, "data/intermediate/spip/nav_spip.csv")
  
  spip <- full_join(nav_spip, price_spip, by = "date")
  spip <- spip %>% mutate(ticker = "SPIP")
  
  write.csv(spip, "data/intermediate/spip/metrics_spip.csv")

# -------------------- Append all ETFs -------------------- #
  tips_etfs_metrics <- bind_rows(tip, stip, ltpz, stpz, tipz, tipx, wip, spip) %>%
    select(date, ticker, price, nav, share_outstanding, volume, aum, high_low)

  tips_etfs_metrics$prem_dis <- (tips_etfs_metrics$price - tips_etfs_metrics$nav) / tips_etfs_metrics$nav * 100
  tips_etfs_metrics$navprice = tips_etfs_metrics$price - tips_etfs_metrics$nav
  tips_etfs_metrics$navprice_scaled = (tips_etfs_metrics$price * tips_etfs_metrics$share_outstanding) - (tips_etfs_metrics$nav * tips_etfs_metrics$share_outstanding)
  
  tips_etfs_metrics <- tips_etfs_metrics %>%
    arrange(ticker, date) %>%   # make sure data is ordered by ETF and date
    group_by(ticker) %>%
    mutate(
      # ETF return from NAV
      nav_ret = (nav - dplyr::lag(nav)) / dplyr::lag(nav),
      price_ret = (price - dplyr::lag(price)) / dplyr::lag(price),
      flows = ((share_outstanding - dplyr::lag(share_outstanding))*nav),
      flow_share = flows / dplyr::lag(aum)
    ) %>%
    ungroup()
  
  tips_etfs_metrics <- tips_etfs_metrics %>%
    mutate(duration = case_when(
      ticker %in% c("TIP", "TIPZ", "TIPX") ~ 7,
      ticker %in% c("STIP", "STPZ") ~ 3,
      ticker %in% c("LTPZ", "SPIP") ~ 15,
      TRUE ~ NA_real_
    ))
  
  write.csv(tips_etfs_metrics, "data/intermediate/tips_etfs/metrics_tips_etfs.csv")


