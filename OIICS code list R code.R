#create list of different OIICS codes
#

library(pacman)
p_load(datasets, ggplot2, data.table, shiny, shinyWidgets, readxl, plyr,stringr)

soc = setDT(read.csv("soc-structure-2000.csv"))

data = data.table(read_excel("../LNI data request/130656-Records.xlsx", sheet = "SF Claims Data"))


#EVENT
oiics_event1 = unique(data.table(CASE_CODE_TYPE = "Event",
CASE_CODE = data$`Oiics Accdt Type Code 1`,
CASE_CODE_TITLE = data$`Oiics Accdt Type Code Desc 1`))
setorder(oiics_event1, CASE_CODE)
oiics_event2 = unique(data.table(CASE_CODE_TYPE = "Event",
                                 CASE_CODE = data$`Oiics Accdt Type Code 2`,
                                 CASE_CODE_TITLE = data$`Oiics Accdt Type Code Desc 2`))
setorder(oiics_event2, CASE_CODE)

#pad 3 digit data
data$`Oiics Accdt Type Code 3` = str_pad(data$`Oiics Accdt Type Code 3`, 3, pad = "0")
oiics_event3 = unique(data.table(CASE_CODE_TYPE = "Event",
                                 CASE_CODE = data$`Oiics Accdt Type Code 3`,
                                 CASE_CODE_TITLE = data$`Oiics Accdt Type Code Desc 3`))
setorder(oiics_event3, CASE_CODE)

remainder = data[`Injry Oiics Accdt Type Code` != data$`Oiics Accdt Type Code 3`,]
oiics_event4 = unique(data.table(CASE_CODE_TYPE = "Event",
                                 CASE_CODE = remainder$`Injry Oiics Accdt Type Code`,
                                 CASE_CODE_TITLE = remainder$`Oiics Accdt Type Code Desc`))
setorder(oiics_event4, CASE_CODE)

oiics_event = rbindlist(list(oiics_event1, oiics_event2, oiics_event3, oiics_event4))
oiics_event = oiics_event[!is.na(CASE_CODE)]
saveRDS(oiics_event,"oiics_event.RDS")

#SOURCE
oiics_source1 = unique(data.table(CASE_CODE_TYPE = "source",
                                 CASE_CODE = data$`Oiics Srce Code 1`,
                                 CASE_CODE_TITLE = data$`Oiics Srce Code Desc 1`))
setorder(oiics_source1, CASE_CODE)
oiics_source2 = unique(data.table(CASE_CODE_TYPE = "source",
                                 CASE_CODE = data$`Oiics Srce Code 2`,
                                 CASE_CODE_TITLE = data$`Oiics Srce Code Desc 2`))
setorder(oiics_source2, CASE_CODE)

#pad 3 digit data
oiics_source3 = unique(data.table(CASE_CODE_TYPE = "source",
                                 CASE_CODE = data$`Oiics Srce Code 3`,
                                 CASE_CODE_TITLE = data$`Oiics Srce Code Desc 3`))
setorder(oiics_source3, CASE_CODE)

remainder = data[!(`Injry Oiics Srce Code` %in% data$`Oiics Srce Code 3`) &
                   !(`Injry Oiics Srce Code` %in% data$`Oiics Srce Code 2`) ,]
oiics_source4 = unique(data.table(CASE_CODE_TYPE = "source",
                                 CASE_CODE = remainder$`Injry Oiics Srce Code`,
                                 CASE_CODE_TITLE = remainder$`Oiics Srce Code Desc`))
setorder(oiics_source4, CASE_CODE)

oiics_source = rbindlist(list(oiics_source1, oiics_source2, oiics_source3, oiics_source4))
oiics_source = oiics_source[!is.na(CASE_CODE)]
saveRDS(oiics_source,"oiics_source.RDS")

#NATURE
oiics_nature1 = unique(data.table(CASE_CODE_TYPE = "nature",
                                  CASE_CODE = data$`Oiics Nat Code 1`,
                                  CASE_CODE_TITLE = data$`Oiics Nat Code Desc 1`))
setorder(oiics_nature1, CASE_CODE)

oiics_nature2 = unique(data.table(CASE_CODE_TYPE = "nature",
                                  CASE_CODE = data$`Oiics Nat Code 2`,
                                  CASE_CODE_TITLE = data$`Oiics Nat Code Desc 2`))
setorder(oiics_nature2, CASE_CODE)

#pad 3 digit data
oiics_nature3 = unique(data.table(CASE_CODE_TYPE = "nature",
                                  CASE_CODE = data$`Oiics Nat Code 3`,
                                  CASE_CODE_TITLE = data$`Oiics Nat Code Desc 3`))
setorder(oiics_nature3, CASE_CODE)

remainder = data[!(`Injry Oiics Nat Code` %in% data$`Oiics Nat Code 3`) &
                   !(`Injry Oiics Nat Code` %in% data$`Oiics Nat Code 2`) &
                   !(`Injry Oiics Nat Code` %in% data$`Oiics Nat Code 1`),]
oiics_nature4 = unique(data.table(CASE_CODE_TYPE = "nature",
                                  CASE_CODE = remainder$`Injry Oiics Nat Code`,
                                  CASE_CODE_TITLE = remainder$`Oiics Nat Code Desc`))
setorder(oiics_nature4, CASE_CODE)

oiics_nature = rbindlist(list(oiics_nature1, oiics_nature2, oiics_nature3, oiics_nature4))
oiics_nature = oiics_nature[!is.na(CASE_CODE)]
saveRDS(oiics_nature,"oiics_nature.RDS")

#BODY PART
oiics_bp1 = unique(data.table(CASE_CODE_TYPE = "bp",
                                  CASE_CODE = data$`Oiics Body Part Code 1`,
                                  CASE_CODE_TITLE = data$`Oiics Body Part Code Desc 1`))
setorder(oiics_bp1, CASE_CODE)

oiics_bp2 = unique(data.table(CASE_CODE_TYPE = "bp",
                                  CASE_CODE = data$`Oiics Body Part Code 2`,
                                  CASE_CODE_TITLE = data$`Oiics Body Part Code Desc 2`))
setorder(oiics_bp2, CASE_CODE)

#pad 3 digit data
oiics_bp3 = unique(data.table(CASE_CODE_TYPE = "bp",
                                  CASE_CODE = data$`Oiics Body Part Code 3`,
                                  CASE_CODE_TITLE = data$`Oiics Body Part Code Desc 3`))
setorder(oiics_bp3, CASE_CODE)

remainder = data[!(`Injry Oiics Body Part Code` %in% data$`Oiics Body Part Code 3`) &
                   !(`Injry Oiics Body Part Code` %in% data$`Oiics Body Part Code 2`) &
                   !(`Injry Oiics Body Part Code` %in% data$`Oiics Body Part Code 1`),]
oiics_bp4 = unique(data.table(CASE_CODE_TYPE = "bp",
                                  CASE_CODE = remainder$`Injry Oiics Body Part Code`,
                                  CASE_CODE_TITLE = remainder$`Oiics Body Part Code Desc`))
setorder(oiics_bp4, CASE_CODE)

oiics_bp = rbindlist(list(oiics_bp1, oiics_bp2, oiics_bp3, oiics_bp4))
oiics_bp = oiics_bp[!is.na(CASE_CODE)]
saveRDS(oiics_bp,"oiics_bp.RDS")





