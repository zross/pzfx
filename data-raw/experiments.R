pzfx_file <- system.file("extdata/exponential_decay.pzfx", package = "pzfx", mustWork = TRUE)
pz <- read_pzfx(pzfx_file, table = 1, strike_action="exclude", date_x="character")

library(tidyr)
library(dplyr)
pz = pz %>%
  pivot_longer(2:last_col(), names_to='Animal', values_to='Value') %>%
  mutate(Group = gsub('_\\d+', '', Animal), Animal=gsub('.*_', '', Animal)) %>%
  arrange()
#
# Y columns are Control and Treated
# Sub columns are 1, 2, 3 for each (the animals)

# pz2 <- list(
#   id = data.frame(id = 1:3),
#   y1 = data.frame(a = list(x1 = 1:3, x2 = 1:3), b = 1:3),
#   y2 = matrix(1:6, ncol = 2)
# )
write_pzfx(pz, path = "~/junk/pz_alt.pzfx")
