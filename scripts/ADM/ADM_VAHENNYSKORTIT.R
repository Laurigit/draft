#ADM_VAHENNYSKORTIT
required_data("STG_VAHENNYSKORTIT")
data <- copy(STG_VAHENNYSKORTIT)
pickit <- data.table(PICK_ORDER = c(1:30))
pickit[, calc_jakaja := STG_VAHENNYSKORTIT[nrow(STG_VAHENNYSKORTIT), jakaja] ^ (ceiling(PICK_ORDER / 2) - 1)]
pickit[, calc_vahennyskortit := STG_VAHENNYSKORTIT[nrow(STG_VAHENNYSKORTIT), vahennyskortit_ykkospick] / calc_jakaja]
ADM_VAHENNYSKORTIT <- pickit[, .(PICK_ORDER, VAHENNYSKORTIT = calc_vahennyskortit)]
