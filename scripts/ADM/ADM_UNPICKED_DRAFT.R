#ADM_UNPICKED_DRAFT

required_data(c("STG_DRAFT_BOOSTER", "STG_DRAFT_PICKORDER"))
omistajat <- data.table(OMISTAJA_ID = c("L", "M"), avain = 1)
ss_DB <- STG_DRAFT_BOOSTER[1 != 0]
ss_DB[, avain := 1]
joinaa_omistaja <- omistajat[ss_DB, on = "avain", allow.cartesian = TRUE][, avain := NULL]

ADM_UNPICKED_DRAFT <- STG_DRAFT_PICKORDER[joinaa_omistaja, on = .(OMISTAJA_ID, Booster_ID, MID)][is.na(PICK_ORDER)]
