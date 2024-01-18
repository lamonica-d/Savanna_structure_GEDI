#raw data visualisation

data_north_congo <- fst::read.fst("rawdata/Northern_Congolian.fst")
data_guinee <- fst::read.fst("rawdata/Guinean_forest-savanna.fst")

par(mfrow= c(2,2))
plot(data_north_congo$fire_freq, data_north_congo$rh98, las = 1)
plot(data_north_congo$fire_freq, data_north_congo$canopy_cover, las = 1)
plot(data_north_congo$mean_precip, data_north_congo$rh98, las = 1)
plot(data_north_congo$mean_precip, data_north_congo$canopy_cover, las = 1)

par(mfrow= c(2,2))
plot(data_guinee$fire_freq[1:400], data_guinee$rh98[1:400], las = 1)
plot(data_guinee$fire_freq[1:400], data_guinee$canopy_cover[1:400], las = 1)
plot(data_guinee$mean_precip[1:400], data_guinee$rh98[1:400], las = 1)
plot(data_guinee$mean_precip[1:400], data_guinee$canopy_cover[1:400], las = 1)


