library(targets)
library(tarchetypes)

# source two seperate targets files:
source("1_fetch.R")
source("2_map.R")
source("3_summarize.R")

c(fetch_targest_list, summarize_targets_list, combine_targets_list, map_targets_list, meta_summaries)
#end  