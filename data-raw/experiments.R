pzfx_file <- system.file("extdata/exponential_decay.pzfx", package = "pzfx", mustWork = TRUE)
pz <- read_pzfx(pzfx_file, table = 1, strike_action="exclude", date_x="character")

library(tidyr)
library(dplyr)
pz <- pz %>%
  pivot_longer(2:last_col(), names_to='Animal', values_to='Value') %>%
  mutate(Group = gsub('_\\d+', '', Animal), Animal=gsub('.*_', '', Animal)) %>%
  arrange()

write_pzfx(pz,
           path = "~/junk/pz_w_grp2.pzfx",
           x_var = "Minutes",
           sub_var = "Animal",
           grp_var = "Group",
           val_var = "Value"
  )
