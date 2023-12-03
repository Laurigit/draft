required_data("SRC_DRAFT_BOOSTER")
draft <- copy(SRC_DRAFT_BOOSTER)

copy_unpicked <- draft
aggr_to_boost <- copy_unpicked[, .N, by = .(Booster_ID, first_pick, random_var)][order(random_var)]
aggr_to_boost[, runneri := seq_len(.N), by = first_pick ]
aggr_to_boost[, new_random := ifelse(first_pick == -1, 1000, runneri + random_var)]
ss_cols <- aggr_to_boost[,. (Booster_ID, sort_order = new_random)]
join_sort <- draft[ss_cols, on = "Booster_ID"]
STG_DRAFT_BOOSTER <- join_sort
