library(targets)
<<<<<<< HEAD

# source the c
source("1_fetch.R")
source("2_process.R")
source("3_visualize.R") # this will include the maps too

# Return the complete list of targets
c(p1_targets_list, p2_targets_list, p3_targets_list)

# End this file with a list of target objects.

# end list

=======
library(tarchetypes)

# source two seperate targets files:
source("1_fetch.R")
source("2_map.R")
source("3_summarize.R")

c(fetch_targest_list, map_targets_list, meta_summaries)
#end
>>>>>>> 3ccdbf1aecf0e4257668c4dd8e48d792ac8ec577
