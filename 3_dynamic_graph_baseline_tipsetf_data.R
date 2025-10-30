setwd("/Users/annaisgangolf/Documents/markets/tips/analysis")

library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)


# ---------------- Dynamically Graph Baseline TIPS ETF Data -------------------- #
# 
#     This script graphs data for TIP, STIP, and LTPZ using the final analysis 
#   data file (metrics_tips_etfs.csv). Time series plots are created for price, volume, 
#   NAV, shares outstanding, AUM, price-NAV, fund flows, and premium/discount. 
#     The first for-loop graphs time series plots for price, volume, 
#   NAV, shares outstanding, AUM, price-NAV, fund flows, and premium/discount for
#   an individual ETF. The list of ETFs graphed depends on those included in the 
#   for-loop "etfs" list. The time period of the graph is determined by the date range 
#   that is initiated. 
#     The second for-loop creates combined plots for a given metric that includes
#   TIP, STIP, TIPZ, STPZ, LTPZ, TIPX, and SPIP. The metrics are initiated in the 
#   for-loop "metric" list. The time period of the graph is determined by the date range 
#   that is iniciated. The title of each graph must be manually changed before the 
#   for-loop is run. 
#
#--- Loan and prepare data ----------------------------------------------------- #
    # Daily TIPS ETF Metric Data
    tipsetfs_data <- read.csv("data/intermediate/tips_etfs/metrics_tips_etfs.csv")
    tipsetfs_data <- tipsetfs_data %>% filter(ticker %in% c("TIP", "STIP", "LTPZ", "STPZ", "TIPZ", "TIPX", "SPIP"))
      tipsetfs_data <- tipsetfs_data %>% mutate(date = as.Date(date))
      tipsetfs_data <- tipsetfs_data %>%
        mutate(duration = case_when(
          ticker %in% c("TIP", "TIPZ", "TIPX") ~ "7",
          ticker %in% c("STIP", "STPZ") ~ "3",
          ticker %in% c("LTPZ", "SPIP") ~ "15",
          TRUE ~ NA_character_
        ))
      
    # Monthly TIPS ETF Metric Data by Ticker
    monthly_tipsetfs_data <- tipsetfs_data %>%
      mutate(month = floor_date(date, "month")) %>%   # convert daily to monthly period
      group_by(ticker, month) %>%
      summarise(
        monthly_flows = mean(flows, na.rm = TRUE),
        monthly_flow_share = mean(flow_share, na.rm = TRUE),
      ) %>%
      ungroup()
    # Monthly TIPS ETF Metric Data by Duration
    monthly_dur_tipsetfs_data <- tipsetfs_data %>%
      mutate(month = floor_date(date, "month")) %>%   # convert daily to monthly period
      group_by(duration, month) %>%
      summarise(
        monthly_flows = mean(flows, na.rm = TRUE),
        monthly_flow_share = mean(flow_share, na.rm = TRUE),
      ) %>%
      ungroup()
    
    # Set Start Date
    sub_date <- as.Date("2018-01-01")
    
#--- Graph Metrics by ETFs ----------------------------------------------------- #
  etfs <- c("tip", "stip", "ltpz", "tipz", "stpz", "tipx", "spip")
    
  for (tick in etfs) {
    etf_sub <- tipsetfs_data %>% filter(ticker == toupper(tick), date >= sub_date)
    print(head(tick))
      
    #--- PRICE -------------------------------------------------- #
      etf_sub %>%
        ggplot(aes(x = date, y = price)) +
        geom_line(color = "blue") +
        labs(title = paste(toupper(tick), ": Price over time"), x = "Time", y = "Price") +
        theme_minimal()
      ggsave(
        filename = paste0("output/", tick, "/baseline/", tick, "_price_", sub_date, ".pdf"),
        width = 8, height = 6
      )
    #--- NAV -------------------------------------------------- #
      etf_sub %>%
        ggplot(aes(x = date, y = nav)) +
        geom_line(color = "blue") +
        labs(title = paste(toupper(tick), ": NAV over time"), x = "Time", y = "NAV") +
        theme_minimal()
      ggsave(
        filename = paste0("output/", tick, "/baseline/", tick, "_nav_", sub_date, ".pdf"),
        width = 8, height = 6
      )
    #--- Volume -------------------------------------------------- #
      etf_sub %>%
        ggplot(aes(x = date, y = volume)) +
        geom_line(color = "blue") +
        labs(title = paste(toupper(tick), ": Volume over time"), x = "Time", y = "Volume") +
        theme_minimal()
      ggsave(
        filename = paste0("output/", tick, "/baseline/", tick, "_volume_", sub_date, ".pdf"),
        width = 8, height = 6
      )
    #--- PRICE-NAV ------------------------------------------------ #
      etf_sub %>%
        ggplot(aes(x = date, y = navprice)) +
        geom_line(color = "blue") +
        labs(title = paste(toupper(tick), ": Price-NAV over time"), x = "Time", y = "Price-NAV") +
        theme_minimal()
      ggsave(
        filename = paste0("output/", tick, "/baseline/", tick, "_navprice_", sub_date, ".pdf"),
        width = 8, height = 6
      )
    #--- Shares Outstanding -------------------------------------- #
      etf_sub %>%
        ggplot(aes(x = date, y = share_outstanding)) +
        geom_line(color = "blue") +
        labs(title = paste(toupper(tick), ": Shares Outstanding over time"), x = "Time", y = "Shares Outstanding") +
        theme_minimal()
      ggsave(
        filename = paste0("output/", tick, "/baseline/", tick, "_shares_outstanding_", sub_date, ".pdf"),
        width = 8, height = 6
      )
    #--- AUM ---------------------------------------------------------- #
      etf_sub %>%
        ggplot(aes(x = date, y = aum)) +
        geom_line(color = "blue") +
        labs(title = paste(toupper(tick), ": AUM over time"), x = "Time", y = "AUM") +
        theme_minimal()
      ggsave(
        filename = paste0("output/", tick, "/baseline/", tick, "_aum_", sub_date, ".pdf"),
        width = 8, height = 6
      )
    #--- Premium/Discount ----------------------------------------------- #
      etf_sub %>%
        ggplot(aes(x = date, y = prem_dis)) +
        geom_line(color = "blue") +
        labs(title = paste(toupper(tick), ": Premium/Discount over time"), x = "Time", y = "Premium/Discount") +
        theme_minimal()
      ggsave(
        filename = paste0("output/", tick, "/baseline/", tick, "_prem_dis_", sub_date, ".pdf"),
        width = 8, height = 6
      )
    #--- Fund Flows as a Share of AUM ------------------------------------ #
      etf_sub %>%
        ggplot(aes(x = date, y = flow_share)) +
        geom_line(color = "blue") +
        labs(title = paste(toupper(tick), ": Fund Flows as a Share of AUM over time"), x = "Time", y = "Fund Flows as a Share of AUM") +
        theme_minimal()
      ggsave(
        filename = paste0("output/", tick, "/baseline/", tick, "_flow_share_", sub_date, ".pdf"),
        width = 8, height = 6
      )
    }
      
#--- Graph ETFs by Metrics ----------------------------------------------------- #
  # Metric List: (price, nav, volume, navprice, share_outstanding, aum, prem_dis, flows, flow_share )
  
  metrics <- c("aum")
  sub_date <- as.Date("2018-01-01")
  
  for (met in metrics) {
    etf_sub <- tipsetfs_data %>% filter(date >= sub_date, ticker %in% c("TIP", "STIP", "LTPZ", "STPZ", "TIPZ", "TIPX", "SPIP"))
    #etf_sub <- monthly_tipsetfs_data %>% filter(month >= sub_date, ticker %in% c("TIP", "STIP", "LTPZ", "STPZ", "TIPZ", "TIPX", "SPIP"))
    #etf_sub <- monthly_dur_tipsetfs_data %>% filter(month >= sub_date)
    print(head(met))
    # Combined TIPS ETFs
    ggplot(etf_sub, aes(x = date, y = .data[[met]], color = ticker)) +
      geom_line(linewidth = .5) +
      scale_color_manual(values = c("TIP"="blue", "STIP"="red", "LTPZ" = "purple", "STPZ" = "lavender", "TIPZ" = "pink", "TIPX" = "skyblue", "SPIP" = "deepskyblue4" )) +
      labs(title = "AUM over time", x = "Time", y = "AUM") +
      theme_minimal()
    ggsave(
      filename = paste0("output/tips_etfs/baseline/tipsetfs_", met,"_", sub_date, ".pdf"),
      width = 8, height = 6
    )
  }
    # Titles to manually insert: 
    #   labs(title = "Price over time", x = "Time", y = "Price") +
    #   labs(title = "NAV over time", x = "Time", y = "NAV") +
    #   labs(title = "AUM over time", x = "Time", y = "AUM") +
    #   labs(title = "Volume over time", x = "Time", y = "Volume") +
    #   labs(title = "Price-NAV over time", x = "Time", y = "Price-NAV") +
    #   labs(title = "Shares Outstanding over time", x = "Time", y = "Shares Outstanding") +
    #   labs(title = "Premium/Discount over time", x = "Time", y = "Premium/Discount") +
    #   labs(title = "Fund Flows as a Share of AUM over time", x = "Time", y = "Fund Flows as a Share of AUM") +
  
    # Graphing TIPS ETFS by Duration
    #   etf_sub <- monthly_tipsetfs_data %>% filter(month >= sub_date, ticker %in% c("TIP", "TIPZ", "TIPX"))
    #   scale_color_manual(values = c("TIP"="blue", "TIPZ" = "pink", "TIPX" = "skyblue")) +
    #   etf_sub <- monthly_tipsetfs_data %>% filter(month >= sub_date, ticker %in% c("STIP", "STPZ"))
    #   scale_color_manual(values = c("STIP"="red", "STPZ" = "lavender")) +
    #   etf_sub <- monthly_tipsetfs_data %>% filter(month >= sub_date, ticker %in% c("LTPZ", "SPIP"))
    #   scale_color_manual(values = c("LTPZ" = "purple", "SPIP" = "deepskyblue4")) +
    #   etf_sub <- monthly_tipsetfs_data %>% filter(month >= sub_date, ticker %in% c("TIP", "STIP", "LTPZ", "STPZ", "TIPZ", "TIPX", "SPIP"))
    #   scale_color_manual(values = c("TIP"="blue", "STIP"="red", "LTPZ" = "purple", "STPZ" = "lavender", "TIPZ" = "pink", "TIPX" = "skyblue", "SPIP" = "deepskyblue4" )) +
