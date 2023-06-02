# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# JUMPSTART: First Sales Analysis ----

# 1.0 Load libraries ----

# Work horse packages
library(tidyverse)
library(lubridate)

# theme_tq()
library(tidyquant)

# Excel Files
library(readxl)
library(writexl)

# 2.0 Importing Files ----
bikes_tbl  <- read_excel(path = "00_data/bike_sales/data_raw/bikes.xlsx")
bikeshops_tbl <- read_excel(path= "00_data/bike_sales/data_raw/bikeshops.xlsx")
orderlines_tbl <- read_excel(path = "00_data/bike_sales/data_raw/orderlines.xlsx")

# 3.0 Examining Data ----
glimpse(bikes_tbl)
glimpse(bikeshops_tbl)
glimpse(orderlines_tbl)

# 4.0 Joining Data ----
left_join(orderlines_tbl, bikes_tbl, by = c("product.id" = "bike.id"))

bike_orderlines_join_tbl <- orderlines_tbl %>%
    left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
    left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))

bike_orderlines_join_tbl %>% glimpse()
# 5.0 Wrangling Data ----
bike_orderlines_wrangled_tbl <- bike_orderlines_join_tbl %>%
    separate(col = description,
             into = c("category.1", "category.2", "frame.material"),
             sep = " - ") %>%
    separate(col = location,
             into = c("city", "state"),
             sep = ", ",
             remove = FALSE) %>%
    mutate(total.price = price * quantity) %>%
    select(-...1, -location) %>%
    select(-ends_with(".id")) %>%
    bind_cols(bike_orderlines_join_tbl %>% select(order.id)) %>%
    select(contains("date"), contains("id"), contains("order"),
           quantity, price, total.price, 
           everything()) %>%
    rename(order_date = order.date) %>%
    set_names(names(.) %>% str_replace_all("\\.", "_")) 

bike_orderlines_wrangled_tbl %>% glimpse()

# 6.0 Business Insights ----


# 6.1 Sales by Year ----

# Step 1 - Manipulate
sales_by_year_tbl <- bike_orderlines_wrangled_tbl %>%
    select(order_date, total_price) %>%
    mutate(year = year(order_date)) %>%
    group_by(year) %>%
    summarize(sales = sum(total_price)) %>%
    ungroup() %>%
    mutate(sales_text = scales::dollar(sales))

# Step 2 - Visualize
sales_by_year_tbl %>%
    ggplot(aes(x = year, y = sales)) +
    geom_col(fill = "#2C3E50") +
    geom_label(aes(label = sales_text)) +
    geom_smooth(method = "lm", se = FALSE) +
    theme_tq() +
    scale_y_continuous(labels = scales::dollar) +
    labs(
        title = "Revenue by Year",
        subtitle = "Upward trend",
        x = "",
        y = "Revenue (USD)"
    )

#palette_light()
# 6.2 Sales by Year and Category 2 ----


# Step 1 - Manipulate
sales_by_year_cat_2_tbl <- bike_orderlines_wrangled_tbl %>%
    select(order_date, total_price, category_2) %>%
    mutate(year = year(order_date)) %>%
    group_by(year, category_2) %>%
    summarize(sales = sum(total_price)) %>%
    ungroup() %>%
    mutate(sales_text = scales::dollar(sales))

# Step 2 - Visualize
sales_by_year_cat_2_tbl %>%
    ggplot(aes(x = year, y = sales, fill = category_2)) +
    geom_col() +
    facet_wrap(~ category_2, ncol = 3, scales = "free_y") + #, scales = "free_y"
    geom_smooth(method = "lm", se = FALSE) +
    theme_tq() +
    scale_fill_tq() +
    scale_y_continuous(labels = scales::dollar) +
    labs(
        title = "Revenue by Year and Category 2",
        subtitle = "Each product category has an upward trend",
        x = "",
        y = "Revenue (USD)",
        fill = "Product Secondary Category"
    )
    

# 7.0 Writing Files ----
fs::dir_create("00_data/bike_sales/date_wrangled_student")

# 7.1 Excel ----
bike_orderlines_wrangled_tbl %>% 
    write_xlsx("00_data/bike_sales/date_wrangled_student/bike_orderlines.xlsx")

# 7.2 CSV ----
bike_orderlines_wrangled_tbl %>% 
    write_csv("00_data/bike_sales/date_wrangled_student/bike_orderlines.csv")

# 7.3 RDS ----
bike_orderlines_wrangled_tbl %>% 
    write_rds("00_data/bike_sales/date_wrangled_student/bike_orderlines.rds")
