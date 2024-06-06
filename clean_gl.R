# Transform GL to TB ------------------------------------------------------
library(tidyverse)


# COA ---------------------------------------------------------------------
# 1. Break down COA further.

assets <- c("Accounts Receivable", "Advertising Expense", "Auto Loan", "Automobiles & Trucks", "Bank Service Charges", "Business License & Fees", "Car Lease", "Car/Truck Expense", "Car/Truck Expense - Other", "Gas", "Insurance-Auto", "Mileage", "Registration & License", "Repairs & Maintenance", "Company Checking Account", "Company Savings Account", "Computer & Office Equipment", "Conferences and Seminars", "Contributions", "Customer Deposits", "Depreciation Expense", "Direct Labor", "Direct Labor - Other", "Wages - Sales-Inside", "Wages - Warehouse", "Dues and Subscriptions", "Employee Advances", "Freight Costs", "Disability Insurance", "General Liability Insurance", "Insurance", "Insurance - Other", "Life Insurance", "Owner's Health Insurance", "Professional Liability Insuranc", "Worker's Compensation", "Inventory Asset", "Line of Credit", "Machinery & Equipment", "Maintenance/Janitorial", "Marketing Expense", "Meals and Entertainment", "Misc. Revenue", "No accnt", "Office Equipment", "Interest Expense", "Other Expense", "Other Expense - Other", "Early Payment Discounts", "Interest Income", "Other Income", "Other Income - Other", "Packaging Materials", "Payroll Clearing (owner's time)", "Payroll Expenses", "Petty Cash Account", "Postage and Delivery", "Prepaid Insurance", "Prepaid Taxes", "Prepaids", "Prepaids - Other", "Printing and Reproduction", "Professional Development", "Accounting Fees", "Legal Fees", "Payroll Service Fees", "Professional Fees", "Professional Fees - Other", "Promotional Expense", "Purchases  (Cost of Goods)", "QuickBooks Credit Card", "Rent", "Computer Repairs", "Equipment Repairs", "Repairs", "Repairs - Other", "Security Deposits", "Supplies", "Telephone", "Travel", "Undeposited Funds", "Utilities", "Vendor Refunds", "Employee Benefits", "Employee Bonus", "Payroll Tax Expenses", "Sick/Holiday & Vacation Pay", "Wages", "Wages - Office Staff", "Wages - Other")

liabilities <- c("Accounts Payable", "Accumulated Depreciation", "401K Payable", "Payroll Liabilities", "Payroll Liabilities - Other", "Payroll Taxes Payable", "SEC125 Payable", "Revenue", "Sales Commission (outside reps)")

equity <- c("Deborah Wood Draws", "Deborah Wood Equity", "Deborah Wood Equity - Other", "Deborah Wood Investment", "Deborah Wood's Time to Jobs", "Opening Bal Equity", "Retained Earnings")


# Import raw data ---------------------------------------------------------
df <- read_csv(("https://raw.githubusercontent.com/stewartli/auditworkpaper/master/data/gl.csv"))

df %>%
  dplyr::filter(!is.na(type)) %>%
  summarise(across(c(debit, credit), sum)) %>%              # check: debit == credit
  mutate(check  = debit - credit)


# TB format ---------------------------------------------------------------
df_bf <- df %>%
  dplyr::filter(!str_detect(account, "^Total|^TOTAL"), !str_detect(subaccount, "^Total|^TOTAL")) %>%
  group_by(account, subaccount) %>%
  slice_head(n = 1) %>%                                     # its format: bf, mov, total,
  ungroup() %>%
  select(account, subaccount, bf = balance) %>%
  mutate(bf = ifelse(subaccount == "Prepaids", 0, bf))      # human error: Insurance vs Prepaids, 800,

df_mov <- df %>%
  dplyr::filter(!str_detect(account, "^Total|^TOTAL"), !str_detect(subaccount, "^Total|^TOTAL")) %>%
  group_by(account, subaccount) %>%
  mutate(coa = rle(subaccount)$lengths) %>%
  mutate(rn = row_number() == 1) %>%
  dplyr::filter(coa == 1 | (coa >= 2 & rn == "FALSE")) %>%
  summarise(across(c(debit, credit), sum), .groups = "drop")

df_clean <- left_join(df_bf, df_mov, by = join_by(account, subaccount)) %>%
  mutate(lib = case_when(subaccount %in% assets ~ "assets",
                         subaccount %in% liabilities ~ "liabilities",
                         subaccount %in% equity ~ "equity" )) %>%
  mutate(cf = case_when(
    lib == "assets" ~ bf + debit - credit,
    lib == "liabilities" ~ bf + debit - credit,
    lib == "equity" ~ bf + debit - credit))                 # recompute

df_clean %>%
  summarise(across(c(debit, credit), sum)) %>%
  mutate(check  = debit - credit)

round(sum(df_clean$cf), 2)                                  # check the equal formula
df_clean %>% summarise(across(c(cf), sum), .by = lib)

12349.00 + 480976.45 - 403171.1 = 90154.32                  # check accuracy - Company Checking Account
2760.00 + 4272.00 +6875.00 +2782.08 - 16689.08              # human error - Prepaids


# Filter out cf -----------------------------------------------------------
df_cf <- df %>%
  group_by(account, subaccount) %>%
  mutate(coa = rle(subaccount)$lengths) %>%
  mutate(rn = row_number() == 1) %>%
  ungroup() %>%
  dplyr::filter(!str_detect(account, "^TOTAL")) %>%
  dplyr::filter(coa == 1 & str_detect(subaccount, "^Total") | (coa == 1 & rn == "TRUE")) %>%
  modify_if(is.character, function(x){str_remove_all(x, "(^Total) ")}) %>%
  mutate(account = ifelse(account == "no accnt", "No accnt", account),
         subaccount = ifelse(subaccount == "no accnt", "No accnt", subaccount)) %>%
  select(account, subaccount, debit, credit, cf = balance) %>%
  distinct(account, subaccount, .keep_all = TRUE) %>%
  mutate(cf = ifelse(subaccount == "Prepaids", 0, cf)) %>%
  mutate(lib = case_when(subaccount %in% assets ~ "assets",
                         subaccount %in% liabilities ~ "liabilities",
                         subaccount %in% equity ~ "equity"))

df_cf %>%
  summarise(across(c(debit, credit), sum)) %>%
  mutate(check  = debit - credit)

round(sum(df_cf$cf), 2)
df_cf %>% summarise(across(c(cf), sum), .by = lib)
setdiff(df_mov$subaccount, df_bf$subaccount)
setdiff(df_mov$subaccount, df_cf$subaccount)
setdiff(df_bf$subaccount, df_cf$subaccount)
setdiff(df_cf$cf, df_clean$cf)                              # different order


# Monthly TB format -------------------------------------------------------
df_mov_mth <- df %>%
  dplyr::filter(!str_detect(account, "^Total|^TOTAL"), !str_detect(subaccount, "^Total|^TOTAL")) %>%
  group_by(account, subaccount) %>%
  mutate(coa = rle(subaccount)$lengths) %>%
  mutate(rn = row_number() == 1) %>%
  ungroup() %>%
  dplyr::filter(coa == 1 | (coa >= 2 & rn == "FALSE")) %>%   # transaction level data
  mutate(mth = lubridate::month(date)) %>%
  replace_na(list(mth = 1)) %>%                              # month: Jan
  group_by(account, subaccount, mth) %>%
  summarise(across(c(debit, credit), sum), .groups = "drop")

df_mov_mth_clean <- map(
  1:12,
  function(x){
    df_mov_mth %>%
      dplyr::filter(mth == x) %>%                             # some account has no transaction over the period
      full_join(., df_bf, by = join_by(account, subaccount)) %>%
      select(account, subaccount, bf, mth, debit, credit) %>%
      fill(mth, .direction = "down") %>%
      replace_na(list(debit = 0, credit = 0)) %>%
      arrange(account, subaccount)                            # for cbind
  }) %>%
  reduce(cbind) %>%                                           # same colname
  janitor::clean_names() %>%
  select(-matches("^[abs].*_\\d+?$")) %>%
  rowwise() %>%
  mutate(dr = sum(c_across(contains("debit"))),
         cr = sum(c_across(contains("credit"))),
         cf = bf + dr - cr) %>%
  ungroup() %>%
  mutate(lib = case_when(subaccount %in% assets ~ "assets",
                         subaccount %in% liabilities ~ "liabilities",
                         subaccount %in% equity ~ "equity"))

df_mov_mth_clean %>%
  summarise(across(c(dr, cr), sum)) %>%
  mutate(check  = dr - cr)

round(sum(df_mov_mth_clean$cf), 2)
df_mov_mth_clean %>% summarise(across(c(cf), sum), .by = lib)
setdiff(df_clean$subaccount, df_mov_mth_clean$subaccount)


# Further analysis --------------------------------------------------------
# 1. Calculate monthly cf based on the current month or accumulated amount.
# 2. Compute MOM%.
# 3. Produce monthly cf (res = 0) based on balance/pl items.

df_fur <- df_mov_mth_clean %>%
  mutate(cf1 = debit - credit,
         cf2 = debit_2 - credit_2,
         cf3 = debit_3 - credit_3,
         cf4 = debit_4 - credit_4,
         cf5 = debit_5 - credit_5,
         cf6 = debit_6 - credit_6,
         cf7 = debit_7 - credit_7,
         cf8 = debit_8 - credit_8,
         cf9 = debit_9 - credit_9,
         cf10 = debit_10 - credit_10,
         cf11 = debit_11 - credit_11,
         cf12 = debit_12 - credit_12) %>%
  mutate(cf_check = bf + cf1 + cf2 + cf3 + cf4 + cf5 + cf6 + cf7 + cf8 + cf9 + cf10 + cf11 + cf12)

round(sum(df_fur$cf_check), 2)
df_fur %>% summarise(across(c(cf, cf_check), ~round(sum(.x))))

df_plot <- df_fur %>%
  select(account, subaccount, bf, contains("cf"), -cf_check) %>%
  rename(bopen = bf, bclose = cf) %>%
  pivot_longer(cols = contains("cf"), names_to = "fcf", values_to = "fcfval") %>%
  mutate(fcf = factor(fcf,
                      levels = c('cf1', 'cf2', 'cf3', 'cf4', 'cf5', 'cf6',
                                 'cf7', 'cf8', 'cf9', 'cf10', 'cf11', 'cf12')))

df_plot %>%
  dplyr::filter(subaccount %in% c("Accounts Payable", "Accounts Receivable",
                                  "Revenue", "Purchases  (Cost of Goods)")) %>%
  ggplot(aes(fcf, fcfval, color = subaccount, group = subaccount)) +
  geom_line() +
  geom_point() +
  ggthemes::theme_wsj()


# Export data -------------------------------------------------------------
writexl::write_xlsx(
  list("recompute" = df_clean, "extract" = df_cf, "monthly" = df_mov_mth_clean),
  here::here("pyshiny/clean_gl/res.xlsx"))

# difference for python pandas: write_csv vs write.csv
write.csv(df_plot, here::here("pyshiny/clean_gl/df_plot1.csv"))























