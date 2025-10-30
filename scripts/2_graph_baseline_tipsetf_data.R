setwd("/Users/annaisgangolf/Documents/markets/tips/analysis")

library(readr)
library(dplyr)
library(ggplot2)

# ----------------------- Graph Baseline TIPS ETF Data ------------------------ #
# 
#     This script graphs data for TIP, STIP, and LTPZ using the final analysis 
#   data file (metrics_tips_etfs.csv). Time series plots are created for price, volume, 
#   NAV, shares outstanding, AUM, price-NAV, fund flows, and premium/discount. 
#     The analysis date file is filtered into two data frames of different date 
#   ranges- 2012-2025 and 2018-2025- and used to create graphs for the two periods.
#   A graph is created for each ETF by each metric for 2012-2025 and 2018-2025. 
#   Graphs are also created containing all ETFs for each metric for 2012-2025 and 
#   2018-2025. These same graphs can be created for TIP, STIP, TIPZ, STPZ, LTPZ, TIPX, 
#   and SPIP using a dynamic file (3_dynamic_graph_baseline_tipsetf.R) that streamlines
#   the graphing process to reduce code. 
  
#--- Load data ---------------------------------------------------------------
tipsetfs_data <- read_csv("data/intermediate/tips_etfs/metrics_tips_etfs.csv")

tipsetfs_data <- tipsetfs_data %>% mutate(date = as.Date(date))

#tipsetfs_data_12 <- tipsetfs_data %>% filter(date >= ("2012-01-01"))
tipsetfs_data_12 <- tipsetfs_data %>% filter(date >= ("2012-01-01"), ticker %in% c("TIP", "STIP", "LTPZ", "STPZ", "TIPZ", "TIPX", "SPIP"))
#tipsetfs_data_18 <- tipsetfs_data %>% filter(date >= ("2018-01-01"))
tipsetfs_data_18 <- tipsetfs_data %>% filter(date >= ("2017-01-01"), ticker %in% c("TIP", "STIP", "LTPZ", "STPZ", "TIPZ", "TIPX", "SPIP"))


#--- PRICE -------------------------------------------------------------------
# Combined TIPS ETFs
    ggplot(tipsetfs_data_18, aes(x = date, y = price, color = ticker)) +
      geom_line(size = 1) +
      scale_color_manual(values = c("TIP" = "blue", "STIP" = "red", "LTPZ" = "purple")) +
      labs(title = "Price over time", x = "Time", y = "Price") +
      theme_minimal()
    ggsave("output/tips_etfs/tipsetfs_price_18-25.pdf", width = 8, height = 6)
    ggplot(tipsetfs_data_12, aes(x = date, y = price, color = ticker)) +
      geom_line(size = 1) +
      scale_color_manual(values = c("TIP" = "blue", "STIP" = "red", "LTPZ" = "purple")) +
      labs(title = "Price over time", x = "Time", y = "Price") +
      theme_minimal()
    ggsave("output/tips_etfs/tipsetfs_price_12-25.pdf", width = 8, height = 6)
# TIP
    tipsetfs_data_18 %>%
      filter(ticker == "TIP") %>%
      ggplot(aes(x = date, y = price)) +
      geom_line(color = "blue") +
      labs(title = "TIP: Price over time", x = "Time", y = "Price") +
      theme_minimal()
    ggsave("output/tip/tip_price_18-25.pdf", width = 8, height = 6)
    tipsetfs_data_12 %>%
      filter(ticker == "TIP") %>%
      ggplot(aes(x = date, y = price)) +
      geom_line(color = "blue") +
      labs(title = "TIP: Price over time", x = "Time", y = "Price") +
      theme_minimal()
    ggsave("output/tip/tip_price_12-25.pdf", width = 8, height = 6)
# STIP 
    tipsetfs_data_18 %>%
      filter(ticker == "STIP") %>%
      ggplot(aes(x = date, y = price)) +
      geom_line(color = "blue") +
      labs(title = "STIP: Price over time", x = "Time", y = "Price") +
      theme_minimal()
    ggsave("output/stip/stip_price_18-25.pdf", width = 8, height = 6)
    tipsetfs_data_12 %>%
      filter(ticker == "STIP") %>%
      ggplot(aes(x = date, y = price)) +
      geom_line(color = "blue") +
      labs(title = "STIP: Price over time", x = "Time", y = "Price") +
      theme_minimal()
    ggsave("output/stip/stip_price_12-25.pdf", width = 8, height = 6)
# LTPZ 
    tipsetfs_data_18 %>%
      filter(ticker == "LTPZ") %>%
      ggplot(aes(x = date, y = price)) +
      geom_line(color = "blue") +
      labs(title = "LTPZ: Price over time", x = "Time", y = "Price") +
      theme_minimal()
    ggsave("output/ltpz/ltpz_price_18-25.pdf", width = 8, height = 6)
    tipsetfs_data_12 %>%
      filter(ticker == "LTPZ") %>%
      ggplot(aes(x = date, y = price)) +
      geom_line(color = "blue") +
      labs(title = "LTPZ: Price over time", x = "Time", y = "Price") +
      theme_minimal()
    ggsave("output/ltpz/ltpz_price_12-25.pdf", width = 8, height = 6)

#--- NAV ---------------------------------------------------------------------
  # Combined TIPS ETFs
    ggplot(tipsetfs_data_12, aes(x = date, y = nav, color = ticker)) +
      geom_line(size = 1) +
      scale_color_manual(values = c("TIP" = "blue", "STIP" = "red", "LTPZ" = "purple")) +
      labs(title = "NAV over time", x = "Time", y = "NAV") +
      theme_minimal()
    ggsave("output/tips_etfs/tipsetfs_nav_18-25.pdf", width = 8, height = 6)
    ggplot(tipsetfs_data_18, aes(x = date, y = nav, color = ticker)) +
      geom_line(size = 1) +
      scale_color_manual(values = c("TIP" = "blue", "STIP" = "red", "LTPZ" = "purple")) +
      labs(title = "NAV over time", x = "Time", y = "NAV") +
      theme_minimal()
    ggsave("output/tips_etfs/tips_etfs_nav_12-25.pdf", width = 8, height = 6)
  # TIP
    tipsetfs_data_18 %>%
      filter(ticker == "TIP") %>%
      ggplot(aes(x = date, y = nav)) +
      geom_line(color = "blue") +
      labs(title = "TIP: NAV over time", x = "Time", y = "NAV") +
      theme_minimal()
    ggsave("output/tip/tip_nav_18-25.pdf", width = 8, height = 6)
    tipsetfs_data_12 %>%
      filter(ticker == "TIP") %>%
      ggplot(aes(x = date, y = nav)) +
      geom_line(color = "blue") +
      labs(title = "TIP: NAV over time", x = "Time", y = "NAV") +
      theme_minimal()
    ggsave("output/tip/tip_nav_12-25.pdf", width = 8, height = 6)
   # STIP
    tipsetfs_data_18 %>%
      filter(ticker == "STIP") %>%
      ggplot(aes(x = date, y = nav)) +
      geom_line(color = "blue") +
      labs(title = "STIP: NAV over time", x = "Time", y = "NAV") +
      theme_minimal()
    ggsave("output/stip/stip_nav_18-25.pdf", width = 8, height = 6)
    tipsetfs_data_12 %>%
      filter(ticker == "TIP") %>%
      ggplot(aes(x = date, y = nav)) +
      geom_line(color = "blue") +
      labs(title = "STIP: NAV over time", x = "Time", y = "NAV") +
      theme_minimal()
    ggsave("output/stip/stip_nav_12-25.pdf", width = 8, height = 6)
  # LTPZ
    tipsetfs_data_18 %>%
      filter(ticker == "LTPZ") %>%
      ggplot(aes(x = date, y = nav)) +
      geom_line(color = "blue") +
      labs(title = "LTPZ: NAV over time", x = "Time", y = "NAV") +
      theme_minimal()
    ggsave("output/ltpz/ltpz_nav_18-25.pdf", width = 8, height = 6)
    tipsetfs_data_12 %>%
      filter(ticker == "LTPZ") %>%
      ggplot(aes(x = date, y = nav)) +
      geom_line(color = "blue") +
      labs(title = "LTPZ: NAV over time", x = "Time", y = "NAV") +
      theme_minimal()
    ggsave("output/ltpz/ltpz_nav_12-25.pdf", width = 8, height = 6)

#--- NAV - PRICE -------------------------------------------------------------
  # Combine TIPS ETFs
    ggplot(tipsetfs_data_18, aes(x = date, y = navprice, color = ticker)) +
      geom_line(size = 1) +
      scale_color_manual(values = c("TIP" = "blue", "STIP" = "red", "LTPZ" = "purple")) +
      labs(title = "Price - NAV over time", x = "Time", y = "Price - NAV") +
      theme_minimal()
    ggsave("output/tips_etfs/tipsetfs_price-nav_18-25.pdf", width = 8, height = 6)
    ggplot(tipsetfs_data_12, aes(x = date, y = navprice, color = ticker)) +
      geom_line(size = 1) +
      scale_color_manual(values = c("TIP" = "blue", "STIP" = "red", "LTPZ" = "purple")) +
      labs(title = "Price - NAV over time", x = "Time", y = "Price - NAV") +
      theme_minimal()
    ggsave("output/tips_etfs/tipsetfs_price-nav_12-25.pdf", width = 8, height = 6)
  # TIP 
    tipsetfs_data_18 %>%
      filter(ticker == "TIP") %>%
      ggplot(aes(x = date, y = navprice)) +
      geom_line(color = "blue") +
      labs(title = "TIP: Price-NAV over time", x = "Time", y = "NAV") +
      theme_minimal()
    ggsave("output/tip/tip_price-nav_18-25.pdf", width = 8, height = 6)
    tipsetfs_data_12 %>%
      filter(ticker == "TIP") %>%
      ggplot(aes(x = date, y = navprice)) +
      geom_line(color = "blue") +
      labs(title = "TIP: Price-NAV over time", x = "Time", y = "NAV") +
      theme_minimal()
    ggsave("output/tip/tip_price-nav_12-25.pdf", width = 8, height = 6)
  # STIP 
    tipsetfs_data_18 %>%
      filter(ticker == "STIP") %>%
      ggplot(aes(x = date, y = navprice)) +
      geom_line(color = "blue") +
      labs(title = "STIP: Price-NAV over time", x = "Time", y = "NAV") +
      theme_minimal()
    ggsave("output/stip/stip_price-nav_18-25.pdf", width = 8, height = 6)
    tipsetfs_data_12 %>%
      filter(ticker == "STIP") %>%
      ggplot(aes(x = date, y = navprice)) +
      geom_line(color = "blue") +
      labs(title = "STIP: Price-NAV over time", x = "Time", y = "NAV") +
      theme_minimal()
    ggsave("output/stip/stip_price-nav_12-25.pdf", width = 8, height = 6)  
  # LTPZ 
    tipsetfs_data_18 %>%
      filter(ticker == "LTPZ") %>%
      ggplot(aes(x = date, y = navprice)) +
      geom_line(color = "blue") +
      labs(title = "LTPZ: Price-NAV over time", x = "Time", y = "NAV") +
      theme_minimal()
    ggsave("output/ltpz/baseline/ltpz_price-nav_18-25.pdf", width = 8, height = 6)
    tipsetfs_data_12 %>%
      filter(ticker == "LTPZ") %>%
      ggplot(aes(x = date, y = navprice)) +
      geom_line(color = "blue") +
      labs(title = "LTPZ: Price-NAV over time", x = "Time", y = "NAV") +
      theme_minimal()
    ggsave("output/ltpz/ltpz_price-nav_12-25.pdf", width = 8, height = 6)

#--- VOLUME ------------------------------------------------------------------
  # Combine TIPS ETFs
    ggplot(tipsetfs_data_18, aes(x = date, y = volume, color = ticker)) +
      geom_line(size = 1) +
      scale_color_manual(values = c("TIP" = "blue", "STIP" = "red", "LTPZ" = "purple")) +
      labs(title = "Volume over time", x = "Time", y = "Volume") +
      theme_minimal()
    ggsave("output/tips_etfs/tipsetfs_volume_18-25.pdf", width = 8, height = 6)
    ggplot(tipsetfs_data_12, aes(x = date, y = navprice, color = ticker)) +
      geom_line(size = 1) +
      scale_color_manual(values = c("TIP" = "blue", "STIP" = "red", "LTPZ" = "purple")) +
      labs(title = "Volume over time", x = "Time", y = "Volume") +
      theme_minimal()
    ggsave("output/tips_etfs/tipsetfs_volume_12-25.pdf", width = 8, height = 6)
   # TIP 
    tipsetfs_data_18 %>%
      filter(ticker == "TIP") %>%
      ggplot(aes(x = date, y = volume)) +
      geom_line(color = "blue") +
      labs(title = "TIP: Volume over time", x = "Time", y = "Volume") +
      theme_minimal()
    ggsave("output/tip/tip_volume_18-25.pdf", width = 8, height = 6)
    tipsetfs_data_12 %>%
      filter(ticker == "TIP") %>%
      ggplot(aes(x = date, y = volume)) +
      geom_line(color = "blue") +
      labs(title = "TIP: Volume over time", x = "Time", y = "Volume") +
      theme_minimal()
    ggsave("output/tip/tip_volume_12-25.pdf", width = 8, height = 6)
  # STIP 
    tipsetfs_data_18 %>%
      filter(ticker == "STIP") %>%
      ggplot(aes(x = date, y = volume)) +
      geom_line(color = "blue") +
      labs(title = "STIP: Volume over time", x = "Time", y = "Volume") +
      theme_minimal()
    ggsave("output/stip/stip_volume_18-25.pdf", width = 8, height = 6)
    tipsetfs_data_12 %>%
      filter(ticker == "STIP") %>%
      ggplot(aes(x = date, y = volume)) +
      geom_line(color = "blue") +
      labs(title = "STIP: Volume over time", x = "Time", y = "Volume") +
      theme_minimal()
    ggsave("output/stip/stip_volume_12-25.pdf", width = 8, height = 6)
  # LTPZ 
    tipsetfs_data_18 %>%
      filter(ticker == "LTPZ") %>%
      ggplot(aes(x = date, y = volume)) +
      geom_line(color = "blue") +
      labs(title = "LTPZ: Volume over time", x = "Time", y = "Volume") +
      theme_minimal()
    ggsave("output/ltpz/ltpz_volume_18-25.pdf", width = 8, height = 6)
    tipsetfs_data_12 %>%
      filter(ticker == "LTPZ") %>%
      ggplot(aes(x = date, y = volume)) +
      geom_line(color = "blue") +
      labs(title = "LTPZ: Volume over time", x = "Time", y = "Volume") +
      theme_minimal()
    ggsave("output/ltpz/ltpz_volume_12-25.pdf", width = 8, height = 6)
    
#--- OUTSTANDING SHARES ------------------------------------------------------
    yoy_share_out <- tipsetfs_data_12 %>%
      filter(date >= as.Date("2017-01-01") & 
               !(ticker == "SPIP" & date < as.Date("2019-09-25"))) %>%
    mutate(year = year(date)) %>%
      group_by(ticker, year) %>%
      summarise(
        year_end_share = last(share_outstanding[order(date)]),  # last available value in that year
        .groups = "drop"
      ) %>%
      group_by(ticker) %>%
      arrange(year) %>%
      mutate(
        yoy_change = (year_end_share / lag(year_end_share) - 1) * 100  # % change
      ) %>%
      ungroup()
    print(yoy_share_out)
    ggplot(yoy_share_out, aes(x = year, y = yoy_change, color = ticker)) +
      geom_line(size = 0.8) +                 # line for each ticker
      geom_point(size = 1.5) +                # dots at each year
      theme_minimal() +
      labs(
        title = "Year-over-Year Change in Shares Outstanding",
        x = "Year",
        y = "YoY Change (%)",
        color = "Ticker"
      )
    yoy_table <- yoy_share_out %>%
      select(ticker, year, yoy_change) %>%
      filter(year %in% c(2020, 2021, 2022, 2023, 2024)) %>%   # keep only years of interest
      pivot_wider(
        names_from = year,
        values_from = yoy_change
      )
    
    print(yoy_table)
    yoy_monthly_share <- tipsetfs_data_18 %>%
      # Keep only TIP, or remove this line if you want all tickers
      #filter(ticker == "TIP") %>%
      filter(!(ticker == "SPIP" & date < as.Date("2019-09-25"))) %>%
      group_by(ticker) %>%
      arrange(date) %>%  # make sure data is in chronological order
      mutate(
        yoy_change = (share_outstanding / lag(share_outstanding, 12) - 1) * 100
      ) %>%
      ungroup()
    ggplot(yoy_monthly_share, aes(x = date, y = yoy_change, color = ticker)) +
      geom_line(size = 0.5) +
      theme_minimal() +
      labs(
        title = "Percentage Change from a Year Ago in Shares Outstanding",
        x = "Date",
        y = "YoY Change (%)",
        color = "Ticker"
      )
    
    monthly_share <- tipsetfs_data_18 %>%
      # Keep only TIP, remove or adjust as needed
      mutate(
        year = year(date),
        month = month(date)
      ) %>%
      group_by(ticker, year, month) %>%
      summarise(
        share_outstanding = mean(share_outstanding, na.rm = TRUE),  # monthly average
        .groups = "drop"
      ) %>%
      # Create a proper date for the month (first day of the month)
      mutate(month_date = as.Date(paste(year, month, "01", sep = "-"))) %>%
      arrange(ticker, month_date) %>%
      group_by(ticker) %>%
      # Step 2: Calculate YoY change (vs same month last year)
      mutate(
        yoy_change = (share_outstanding / lag(share_outstanding, 12) - 1) * 100
      ) %>%
      ungroup()
    ggplot(monthly_share, aes(x = month_date, y = yoy_change, color = ticker)) +
      geom_line(size = 0.5) +        # line for each ticker
      theme_minimal() +
      labs(
        title = "Monthly Year-over-Year Change in Shares Outstanding",
        x = "Month",
        y = "YoY Change (%)",
        color = "Ticker"
      ) +
      scale_x_date(date_labels = "%Y-%m", date_breaks = "3 months") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    dur_monthly_share <- monthly_share %>% filter(ticker %in% c("TIP", "TIPX", "TIPZ"))
    #dur_monthly_share <- monthly_share %>% filter(ticker %in% c("STIP", "STPZ"))
    #dur_monthly_share <- monthly_share %>% filter(ticker %in% c("LTPZ", "SPIP"))
    ggplot(dur_monthly_share, aes(x = month_date, y = yoy_change, color = ticker)) +
      geom_line(size = 0.5) +
      theme_minimal() +
      labs(
        title = "Monthly Year-over-Year Change in Shares Outstanding",
        x = "Month-Year",
        y = "YoY Change (%)",
        color = "Ticker"
      ) +
      scale_x_date(
        date_labels = "%Y",    # show only the year
        date_breaks = "1 year" # place a tick for each year
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")
    
    
    avg_share_out <- tipsetfs_data_18 %>%
      filter(!(ticker == "SPIP" & date < as.Date("2019-09-25"))) %>%
      group_by(ticker) %>%
      summarise(
        avg_share = mean(share_outstanding, na.rm = TRUE),
        sd_share = sd(share_outstanding, na.rm = TRUE)
      ) %>%
      mutate(
        sd_pct_share= (sd_share / avg_share) * 100   # SD as a percentage of avg AUM
      )
    print(avg_share_out)
    # Combine TIPS ETFs
    ggplot(tipsetfs_data_18, aes(x = date, y = share_outstanding, color = ticker)) +
      geom_line(size = 1) +
      scale_color_manual(values = c("TIP" = "blue", "STIP" = "red", "LTPZ" = "purple")) +
      labs(title = "Shares Outstanding over time", x = "Time", y = "Shares Outstanding") +
      theme_minimal()
    ggsave("output/tips_etfs/tipsetfs_sharesoutstanding_18-25.pdf", width = 8, height = 6)
    ggplot(tipsetfs_data_12, aes(x = date, y = share_outstanding, color = ticker)) +
      geom_line(size = 1) +
      scale_color_manual(values = c("TIP" = "blue", "STIP" = "red", "LTPZ" = "purple")) +
      labs(title = "Shares Outstanding over time", x = "Time", y = "Shares Outstanding") +
      theme_minimal()
    ggsave("output/tips_etfs/tipsetfs_sharesoutstanding_12-25.pdf", width = 8, height = 6)
  # TIP 
    tipsetfs_data_18 %>%
      filter(ticker == "TIP") %>%
      ggplot(aes(x = date, y = share_outstanding)) +
      geom_line(color = "blue") +
      labs(title = "TIP: Shares Outstanding over time", x = "Time", y = "Shares Outstanding") +
      theme_minimal()
    ggsave("output/tip/tip_sharesoutstanding_18-25.pdf", width = 8, height = 6)
    tipsetfs_data_12 %>%
      filter(ticker == "TIP") %>%
      ggplot(aes(x = date, y = share_outstanding)) +
      geom_line(color = "blue") +
      labs(title = "TIP: Shares Outstanding over time", x = "Time", y = "Shares Outstanding") +
      theme_minimal()
    ggsave("output/tip/tip_sharesoutstanding_12-25.pdf", width = 8, height = 6)
  # STIP 
    tipsetfs_data_18 %>%
      filter(ticker == "STIP") %>%
      ggplot(aes(x = date, y = share_outstanding)) +
      geom_line(color = "blue") + 
      labs(title = "STIP: Shares Outstanding over time", x = "Time", y = "Shares Outstanding") +
      theme_minimal()
    ggsave("output/stip/stip_sharesoutstanding_18-25.pdf", width = 8, height = 6)
    tipsetfs_data_12 %>%
      filter(ticker == "STIP") %>%
      ggplot(aes(x = date, y = share_outstanding)) +
      geom_line(color = "blue") +
      labs(title = "STIP: Shares Outstanding over time", x = "Time", y = "Shares Outstanding") +
      theme_minimal()
    ggsave("output/stip/stip_sharesoutstanding_12-25.pdf", width = 8, height = 6)
  # LTPZ 
    tipsetfs_data_18 %>%
      filter(ticker == "LTPZ") %>%
      ggplot(aes(x = date, y = share_outstanding)) +
      geom_line(color = "blue") +
      labs(title = "LTPZ: Shares Outstanding over time", x = "Time", y = "Shares Outstanding") +
      theme_minimal()
    ggsave("output/ltpz/ltpz_sharesoutstanding_18-25.pdf", width = 8, height = 6)
    tipsetfs_data_12 %>%
      filter(ticker == "LTPZ") %>%
      ggplot(aes(x = date, y = share_outstanding)) +
      geom_line(color = "blue") +
      labs(title = "LTPZ: Shares Outstanding over time", x = "Time", y = "Shares Outstanding") +
      theme_minimal()
    ggsave("output/ltpz/ltpz_sharesoutstanding_12-25.pdf", width = 8, height = 6)

#--- AUM ------------------------------------------------------
    # Table Average and Standard Deviation 
    avg_sd_by_fund <- tipsetfs_data_18 %>%
        group_by(ticker) %>%
        summarise(
          avg_aum = mean(aum, na.rm = TRUE),
          sd_aum = sd(aum, na.rm = TRUE)
        ) %>%
        mutate(
          sd_pct = (sd_aum / avg_aum) * 100   # SD as a percentage of avg AUM
        )
        print(avg_sd_by_fund)
    # Combine TIPS ETFs
    ggplot(tipsetfs_data_18, aes(x = date, y = aum, color = ticker)) +
      geom_line(size = 1) +
      scale_color_manual(values = c("TIP" = "blue", "STIP" = "red", "LTPZ" = "purple")) +
      labs(title = "AUM over time", x = "Time", y = "AUM") +
      theme_minimal()
    ggsave("output/tips_etfs/tipsetfs_aum_18-25.pdf", width = 8, height = 6)
    ggplot(tipsetfs_data_12, aes(x = date, y = aum, color = ticker)) +
      geom_line(size = 1) +
      scale_color_manual(values = c("TIP" = "blue", "STIP" = "red", "LTPZ" = "purple")) +
      labs(title = "AUM over time", x = "Time", y = "AUM") +
      theme_minimal()
    ggsave("output/tips_etfs/tipsetfs_aum_12-25.pdf", width = 8, height = 6)
    # TIP 
    tipsetfs_data_18 %>%
      filter(ticker == "TIP") %>%
      ggplot(aes(x = date, y = aum)) +
      geom_line(color = "blue") +
      labs(title = "TIP: AUM over time", x = "Time", y = "AUM") +
      theme_minimal()
    ggsave("output/tip/tip_aum_18-25.pdf", width = 8, height = 6)
    tipsetfs_data_12 %>%
      filter(ticker == "TIP") %>%
      ggplot(aes(x = date, y = aum)) +
      geom_line(color = "blue") +
      labs(title = "TIP: AUM over time", x = "Time", y = "AUM") +
      theme_minimal()
    ggsave("output/tip/tip_aum_12-25.pdf", width = 8, height = 6)
    # STIP 
    tipsetfs_data_18 %>%
      filter(ticker == "STIP") %>%
      ggplot(aes(x = date, y = aum)) +
      geom_line(color = "blue") +
      labs(title = "STIP: AUM over time", x = "Time", y = "AUM") +
      theme_minimal()
    ggsave("output/stip/stip_aum_18-25.pdf", width = 8, height = 6)
    tipsetfs_data_12 %>%
      filter(ticker == "STIP") %>%
      ggplot(aes(x = date, y = aum)) +
      geom_line(color = "blue") +
      labs(title = "STIP: AUM over time", x = "Time", y = "AUM") +
      theme_minimal()
    ggsave("output/stip/stip_aum_12-25.pdf", width = 8, height = 6)
    # LTPZ 
    tipsetfs_data_18 %>%
      filter(ticker == "LTPZ") %>%
      ggplot(aes(x = date, y = aum)) +
      geom_line(color = "blue") +
      labs(title = "LTPZ: AUM over time", x = "Time", y = "AUM") +
      theme_minimal()
    ggsave("output/ltpz/ltpz_aum_18-25.pdf", width = 8, height = 6)
    tipsetfs_data_12 %>%
      filter(ticker == "LTPZ") %>%
      ggplot(aes(x = date, y = aum)) +
      geom_line(color = "blue") +
      labs(title = "LTPZ: AUM over time", x = "Time", y = "AUM") +
      theme_minimal()
    ggsave("output/ltpz/ltpz_aum_12-25.pdf", width = 8, height = 6)
    
#--- Premium/Discount ------------------------------------------------------
    # Table Average and Standard Deviation 
    avg_sd_prem_dis <- tipsetfs_data_18 %>%
      filter(!(ticker == "SPIP" & date < as.Date("2019-09-25"))) %>%
      #filter(!(date >= as.Date("2020-04-01") & date <= as.Date("2022-03-01"))) %>%
      #filter(!(date >= as.Date("2022-03-01") & date <= as.Date("2023-09-01"))) %>%
      filter(!(date >= as.Date("2018-01-01") & date <= as.Date("2020-03-01"))) %>%
      group_by(ticker) %>%
      summarise(
        total_days = n(),
        premium_days = sum(prem_dis > 0, na.rm = TRUE),
        pct_premium = (premium_days / total_days) * 100
      )
    print(avg_sd_prem_dis)
    avg_sd_prem_dis <- tipsetfs_data_18 %>%
      filter(!(ticker == "SPIP" & date < as.Date("2019-09-25"))) %>%
        group_by(ticker) %>%
        summarise(
          avg_prem_dis = mean(prem_dis, na.rm = TRUE),
          avg_prem_days = mean(premium_days, na.rm = TRUE),
          sd_prem_dis = sd(prem_dis, na.rm = TRUE)
        ) %>%
        mutate(
          sd_pct_prem_dis = (sd_prem_dis / avg_prem_dis) * 100   # SD as a percentage of avg AUM
        )
      print(avg_sd_prem_dis)
    # Combine TIPS ETFs
    ggplot(tipsetfs_data_18, aes(x = date, y = prem_dis, color = ticker)) +
      geom_line(size = 1) +
      scale_color_manual(values = c("TIP" = "blue", "STIP" = "red", "LTPZ" = "purple")) +
      labs(title = "Premium/Discount over time", x = "Time", y = "Premium/Discount") +
      theme_minimal()
    ggsave("output/tips_etfs/tipsetfs_premdis_18-25.pdf", width = 8, height = 6)
    ggplot(tipsetfs_data_12, aes(x = date, y = prem_dis, color = ticker)) +
      geom_line(size = 1) +
      scale_color_manual(values = c("TIP" = "blue", "STIP" = "red", "LTPZ" = "purple")) +
      labs(title = "Premium/Discount over time", x = "Time", y = "Premium/Discount") +
      theme_minimal()
    ggsave("output/tips_etfs/tipsetfs_prem_dis_12-25.pdf", width = 8, height = 6)
    tipsetfs_data_18 %>%
      filter(date > "2020-04-01") %>%
      ggplot(aes(x = date, y = prem_dis, color = ticker)) +
      geom_line(size = 1) +
      scale_color_manual(values = c("TIP" = "blue", "STIP" = "red", "LTPZ" = "purple")) +
      labs(title = "Premium/Discount over time", x = "Time", y = "Premium/Discount") +
      theme_minimal()
    ggsave("output/tips_etfs/tipsetfs_prem_dis_21-25.pdf", width = 8, height = 6)
    # TIP 
    prem_dis_mean <- mean(tipsetfs_data_18$prem_dis[tipsetfs_data_18$ticker == "TIP"], na.rm = TRUE)
    tipsetfs_data_18 %>%
      filter(ticker == "TIP") %>%
      ggplot(aes(x = date, y = prem_dis)) +
      geom_line(color = "blue") +
      geom_hline(yintercept = prem_dis_mean, linetype = "dashed", color = "darkgreen", size = 1) +
      labs(title = "TIP: Premium/Discount over time", x = "Time", y = "Premium/Discount") +
      theme_minimal()
    ggsave("output/tip/tip_prem_dis_18-25.pdf", width = 8, height = 6)
    tipsetfs_data_12 %>%
      filter(ticker == "TIP") %>%
      ggplot(aes(x = date, y = prem_dis)) +
      geom_line(color = "blue") +
      geom_hline(yintercept = prem_dis_mean, linetype = "dashed", color = "darkgreen", size = 1) +
      labs(title = "TIP: Premium/Discount over time", x = "Time", y = "Premium/Discount") +
      theme_minimal()
    ggsave("output/tip/tip_prem_dis_12-25.pdf", width = 8, height = 6)
    prem_dis_mean <- mean(tipsetfs_data_18$prem_dis[tipsetfs_data_18$ticker == "TIP" & tipsetfs_data_18$date > as.Date("2020-04-01")], na.rm = TRUE)
    tipsetfs_data_18 %>%
      filter(ticker == "TIP") %>%
      filter(date > "2020-04-01") %>%
      ggplot(aes(x = date, y = prem_dis)) +
      geom_line(color = "blue") +
      geom_hline(yintercept = prem_dis_mean, linetype = "dashed", color = "darkgreen", size = 1) +
      labs(title = "TIP: Premium/Discount over time", x = "Time", y = "Premium/Discount") +
      theme_minimal()
    ggsave("output/tip/tip_prem_dis_21-25.pdf", width = 8, height = 6)
    # STIP 
    prem_dis_mean <- mean(tipsetfs_data_18$prem_dis[tipsetfs_data_18$ticker == "STIP"], na.rm = TRUE)
    tipsetfs_data_18 %>%
      filter(ticker == "STIP") %>%
      ggplot(aes(x = date, y = prem_dis)) +
      geom_line(color = "blue") +
      geom_hline(yintercept = prem_dis_mean, linetype = "dashed", color = "darkgreen", size = 1) +
      labs(title = "STIP: Premium/Discount over time", x = "Time", y = "Premium/Discount") +
      theme_minimal()
    ggsave("output/stip/stip_prem_dis_18-25.pdf", width = 8, height = 6)
    tipsetfs_data_12 %>%
      filter(ticker == "STIP") %>%
      ggplot(aes(x = date, y = prem_dis)) +
      geom_line(color = "blue") +
      labs(title = "STIP: Premium/Discount over time", x = "Time", y = "Premium/Discount") +
      theme_minimal()
    ggsave("output/stip/stip_prem_dis_12-25.pdf", width = 8, height = 6)
    prem_dis_mean <- mean(tipsetfs_data_18$prem_dis[tipsetfs_data_18$ticker == "STIP" & tipsetfs_data_18$date > as.Date("2020-04-01")], na.rm = TRUE)
    tipsetfs_data_18 %>%
      filter(ticker == "STIP") %>%
      filter(date > "2020-04-01") %>%
      ggplot(aes(x = date, y = prem_dis)) +
      geom_line(color = "blue") +
      geom_hline(yintercept = prem_dis_mean, linetype = "dashed", color = "darkgreen", size = 1) +
      labs(title = "STIP: Premium/Discount over time", x = "Time", y = "Premium/Discount") +
      theme_minimal()
    ggsave("output/stip/stip_prem_dis_21-25.pdf", width = 8, height = 6)
    # LTPZ 
    prem_dis_mean <- mean(tipsetfs_data_18$prem_dis[tipsetfs_data_18$ticker == "LTPZ"], na.rm = TRUE)
    tipsetfs_data_18 %>%
      filter(ticker == "LTPZ") %>%
      ggplot(aes(x = date, y = prem_dis)) +
      geom_line(color = "blue") +
      geom_hline(yintercept = prem_dis_mean, linetype = "dashed", color = "darkgreen", size = 1) +
      labs(title = "LTPZ: Premium/Discount over time", x = "Time", y = "Premium/Discount") +
      theme_minimal()
    ggsave("output/ltpz/ltpz_prem_dis_18-25.pdf", width = 8, height = 6)
    tipsetfs_data_12 %>%
      filter(ticker == "LTPZ") %>%
      ggplot(aes(x = date, y = prem_dis)) +
      geom_line(color = "blue") +
      labs(title = "LTPZ: Premium/Discount over time", x = "Time", y = "Premium/Discount") +
      theme_minimal()
    ggsave("output/ltpz/ltpz_prem_dis_12-25.pdf", width = 8, height = 6)
    prem_dis_mean <- mean(tipsetfs_data_18$prem_dis[tipsetfs_data_18$ticker == "LTPZ" & tipsetfs_data_18$date > as.Date("2020-04-01")], na.rm = TRUE)
    tipsetfs_data_18 %>% 
      filter(ticker == "LTPZ") %>% 
      filter(date > "2020-04-01") %>%
      ggplot(aes(x = date, y = prem_dis)) +
      geom_line(color = "blue") +
      geom_hline(yintercept = prem_dis_mean, linetype = "dashed", color = "darkgreen", size = 1) +
      labs(title = "LTPZ: Premium/Discount over time", x = "Time", y = "Premium/Discount") +
      theme_minimal()
    ggsave("output/ltpz/ltpz_prem_dis_21-25.pdf", width = 8, height = 6)
    
#--- Fund Flows ------------------------------------------------------
    # Combine TIPS ETFs
    ggplot(tipsetfs_data_18, aes(x = date, y = flow_share, color = ticker)) +
      geom_line(size = 1) +
      scale_color_manual(values = c("TIP" = "blue", "STIP" = "red", "LTPZ" = "purple")) +
      labs(title = "Fund Flows as a Share of AUM over time", x = "Time", y = "Fund Flows as a Share of AUM") +
      theme_minimal()
    ggsave("output/tips_etfs/tipsetfs_premdis_18-25.pdf", width = 8, height = 6)
    ggplot(tipsetfs_data_12, aes(x = date, y = flow_share, color = ticker)) +
      geom_line(size = 1) +
      scale_color_manual(values = c("TIP" = "blue", "STIP" = "red", "LTPZ" = "purple")) +
      labs(title = "Fund Flows as a Share of AUM over time", x = "Time", y = "Fund Flows as a Share of AUM") +
      theme_minimal()
    ggsave("output/tips_etfs/tipsetfs_flow_share_12-25.pdf", width = 8, height = 6)
    # TIP 
    tipsetfs_data_18 %>%
      filter(ticker == "TIP") %>%
      ggplot(aes(x = date, y = flow_share)) +
      geom_line(color = "blue") +
      labs(title = "TIP: Fund Flows as a Share of AUM over time", x = "Time", y = "Fund Flows as a Share of AUM") +
      theme_minimal()
    ggsave("output/tip/tip_flow_share_18-25.pdf", width = 8, height = 6)
    tipsetfs_data_12 %>%
      filter(ticker == "TIP") %>%
      ggplot(aes(x = date, y = flow_share)) +
      geom_line(color = "blue") +
      labs(title = "TIP: Fund Flows as a Share of AUM over time", x = "Time", y = "Fund Flows as a Share of AUM") +
      theme_minimal()
    ggsave("output/tip/tip_flow_share_12-25.pdf", width = 8, height = 6)
    # STIP 
    tipsetfs_data_18 %>%
      filter(ticker == "STIP") %>%
      ggplot(aes(x = date, y = flow_share)) +
      geom_line(color = "blue") +
      labs(title = "STIP: Fund Flows as a Share of AUM over time", x = "Time", y = "Fund Flows as a Share of AUM") +
      theme_minimal()
    ggsave("output/stip/stip_flow_share_18-25.pdf", width = 8, height = 6)
    tipsetfs_data_12 %>%
      filter(ticker == "STIP") %>%
      ggplot(aes(x = date, y = flow_share)) +
      geom_line(color = "blue") +
      labs(title = "STIP: Fund Flows as a Share of AUM over time", x = "Time", y = "Fund Flows as a Share of AUM") +
      theme_minimal()
    ggsave("output/stip/stip_flow_share_12-25.pdf", width = 8, height = 6)
    # LTPZ 
    tipsetfs_data_18 %>%
      filter(ticker == "LTPZ") %>%
      ggplot(aes(x = date, y = flow_share)) +
      geom_line(color = "blue") +
      labs(title = "LTPZ: Fund Flows as a Share of AUM over time", x = "Time", y = "Fund Flows as a Share of AUM") +
      theme_minimal()
    ggsave("output/ltpz/ltpz_flow_share_18-25.pdf", width = 8, height = 6)
    tipsetfs_data_12 %>%
      filter(ticker == "LTPZ") %>%
      ggplot(aes(x = date, y = flow_share)) +
      geom_line(color = "blue") +
      labs(title = "LTPZ: Fund Flows as a Share of AUM over time", x = "Time", y = "Fund Flows as a Share of AUM") +
      theme_minimal()
    ggsave("output/ltpz/ltpz_flow_share_12-25.pdf", width = 8, height = 6)
    
    
    
    
    
    
