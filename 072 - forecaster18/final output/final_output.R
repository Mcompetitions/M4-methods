


c_var <- function(x){(x[,c('median.f')])}

## Yearly
yearly_output <- lapply(for.yearly.m4,c_var)
table.yearly <- as.data.table(t(as.data.table(yearly_output)),keep.rownames=T)


## Quarterly
quarterly_output <- lapply(for.quarterly.m4,c_var)
table.quarterly <- as.data.table(t(as.data.table(quarterly_output)),keep.rownames=T)

## Monthly
monthly_output <- lapply(for.monthly.m4,c_var)
table.monthly <- as.data.table(t(as.data.table(monthly_output)),keep.rownames=T)

## weekly
weekly_output <- lapply(for.weekly.m4.df,c_var)
table.weekly <- as.data.table(t(as.data.table(weekly_output)),keep.rownames=T)

# Hourly
hourly_output <- lapply(for.hourly.m4,c_var)
table.hourly <- as.data.table(t(as.data.table(hourly_output)),keep.rownames=T)


mx <- rbind.fill(table.yearly,table.quarterly,table.monthly,table.weekly,table.hourly)


