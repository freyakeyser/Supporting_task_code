load("C:/Users/keyserf/Documents/temp_data/Data/Survey_data/2025/Survey_summary_output/Survey_all_results.Rdata")

require(tidyverse)

datebank <- all.surv.dat[all.surv.dat$year>2009,] %>%
  group_by(bank, survey, year) %>%
  summarize(start = min(date), 
            end = max(date))

datebank$days <- datebank$end - datebank$start

require(ggplot2)

datebank$start <- paste0("2025-", month(datebank$start), "-", day(datebank$start))
datebank$end <- paste0("2025-", month(datebank$end), "-", day(datebank$end))

datemed <- datebank %>%
  group_by(bank) %>%
  summarize(med = median(ymd(start)),
            days = median(days))

datebank[datebank$year==2025,]

ggplot() + 
  geom_segment(data=datebank[datebank$year>2010,], aes(x = ymd(start), xend=ymd(end), year), colour="black") + 
  geom_point(data=datebank[datebank$year>2010,], aes(ymd(start), year), colour="forestgreen") + 
  geom_point(data=datebank[datebank$year>2010,], aes(ymd(end), year), colour="red") + 
  geom_vline(data=datemed, aes(xintercept = ymd(med))) +
  scale_x_date(date_labels = "%b-%d") +
  facet_wrap(~bank, scale="free_x")

#Start GBMon on 2026-05-26 (1 day)
#Start BBn on 2026-05-28 (5 days)
#Start Ger on 2026-06-04 (9 days)