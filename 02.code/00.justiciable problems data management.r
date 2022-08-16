# 0. libraries   -----------------------------------------------------------------------------------
options(scipen=999)

Sys.setenv(http_proxy='http://staff-proxy.ul.ie:8080')
Sys.getenv('HTTPS_PROXY')

lib <- c('tidyr','plyr', 'ggplot2','viridis','dplyr',
         'forcats','hrbrthemes','data.table','curl',
         'readxl','foreign','ggalt','viridis','Hmisc',
         'forcats','tidyverse')

lapply(lib, library, character.only = TRUE);rm(lib)

#  a. general data tables  -------------------------------------------------------------------------
# * a.1. microdata list  ---------------------------------------------------------------------------

#lista de archivos 
a1_fil  <- '13ef5yuDPVOl1aOzTwTE3os-p-vljBv5e'
a1_pat  <- 'https://docs.google.com/uc?id=%s&export=download'
a1_tab <- data.frame(data.table::fread(sprintf(a1_pat, a1_fil,
                                               download.method = 'curl'), 
                                       header = TRUE,sep = ';'))

#data loading function
a1_f1 <- function(x){data.table::fread(sprintf(a1_pat,x),header = TRUE)}

#data denomination vector
a1_nam <- a1_tab$name

#ECSC 2020 table list
a1_lta <- lapply(a1_tab$file_id,a1_f1)
names(a1_lta)  <- a1_nam

# * a.2. Module selection --------------------------------------------------------------------------

# * * * * household and individual tables ----------------------------------------------------------
a2_01 <- a1_lta[[ 1]] # 01.household data
a2_03 <- a1_lta[[ 3]] # 03.security perception
a2_05 <- a1_lta[[ 5]] # 05.crime filter
a2_04 <- a1_lta[[ 4]] # 04.individuals characteristics 1
a2_06 <- a1_lta[[ 6]] # 06.residency theft P1228
a2_07 <- a1_lta[[ 7]] # 07 animal theft P2073
a2_08 <- a1_lta[[ 8]] # 08.vehicle theft P1238
a2_09 <- a1_lta[[ 9]] # 09.theft P1324
a2_11 <- a1_lta[[11]] # 11.extortion P1187
a2_12 <- a1_lta[[12]] # 12.security contribution
a2_16 <- a1_lta[[16]] # 16.individuals characteristics 2

# * * * * legal need tables ------------------------------------------------------------------------
# a2_13 <- a1_lta[[13]]; a2_13$FEX_C <- as.numeric(gsub(',','.',a2_13$FEX_C)) ## 13.pdcd long cicle 1
# a2_14 <- a1_lta[[14]]; a2_14$FEX_C <- as.numeric(gsub(',','.',a2_14$FEX_C)) ## 14.pdcd long cicle 2
# a2_15 <- a1_lta[[15]]; a2_15$FEX_C <- as.numeric(gsub(',','.',a2_15$FEX_C)) ## 15.pdcd short cicle
# 

# * a.3. control selection  ------------------------------------------------------------------------

# Censal Multi dimensional poverty index
a3_ipm <- data.table::fread(sprintf(a1_pat, '16klmRUF0wD-Iv7QdbYEauIPvMsNKFq1w',
                                    download.method = 'curl'),
                            header = TRUE, encoding = 'UTF-8')

# Population records 
a3_pob <- openxlsx::read.xlsx(sprintf(a1_pat, '1CiAXTmZzM5SoXOjf8jq0rm0s1SE4X3IW',
                                      download.method = 'curl'))
a3_pob$DPMP <- as.numeric(a3_pob$DPMP)

# Social pulse Nov 2021 city records
a3_wb <- openxlsx::read.xlsx('L:/99.LSE studies/99.Dissertation/01.data/03.wellbeign/99.wb_tab.xlsx')

# SIEDCO
a4_wb <- openxlsx::read.xlsx('L:/99.LSE studies/99.Dissertation/01.data/05.SIEDCO/99.siedco_tab.xlsx')

# SICAAC
a5_wb <- openxlsx::read.xlsx('L:/99.LSE studies/99.Dissertation/01.data/04.SICAAC/99.sicaac_tab.xlsx')

# Justicie supply
a6_wb <- openxlsx::read.xlsx('L:/99.LSE studies/99.Dissertation/01.data/06.justice_supply/99.js_tab.xlsx')

# * a.4. labels  -----------------------------------------------------------------------------------

a4_lab1 <- openxlsx::read.xlsx(sprintf(a1_pat, '1qDlN3gw2T93D5HzGtDT6qZ01YIN-b8v_',
                                       download.method = 'curl'),sheet = 4)
a4_lab2 <- openxlsx::read.xlsx(sprintf(a1_pat, '1Ea4MZjz5qgbBP7GgTto1_0GbohGFtgeX',
                                       download.method = 'curl'),sheet = 3)
a4_lab3 <- openxlsx::read.xlsx(sprintf(a1_pat, '1zaCCV3ZayGOjKeH6k38j2kmOcc4Km4wB',
                                       download.method = 'curl'),sheet = 6)

path_rutas <- ('L:/01.ecsc2020/01.tablas/lab_rutas.xlsx')

a4_labP1672 <- openxlsx::read.xlsx(sprintf(path_rutas),sheet = 'P1672')
a4_labP1675 <- openxlsx::read.xlsx(sprintf(path_rutas),sheet = 'P1675')
a4_labP1676 <- openxlsx::read.xlsx(sprintf(path_rutas),sheet = 'P1676')
a4_labP1676S3 <- openxlsx::read.xlsx(sprintf(path_rutas),sheet = 'P1676S3')
a4_labP1677 <- openxlsx::read.xlsx(sprintf(path_rutas),sheet = 'P1677')
a4_labP1678 <- openxlsx::read.xlsx(sprintf(path_rutas),sheet = 'P1678')
a4_labP1679 <- openxlsx::read.xlsx(sprintf(path_rutas),sheet = 'P1679')
a4_labP1681 <- openxlsx::read.xlsx(sprintf(path_rutas),sheet = 'P1681')
a4_labP1682 <- openxlsx::read.xlsx(sprintf(path_rutas),sheet = 'P1682')
a4_labP1683 <- openxlsx::read.xlsx(sprintf(path_rutas),sheet = 'P1683')
a4_labP1674 <- openxlsx::read.xlsx(sprintf(path_rutas),sheet = 'P1674')

# * * * * pre-reshaped legal need tables  ----------------------------------------------------------

dt_fil  <- '1y9ATpg7NrwqitwMEm5BbXvnLWlb8Xyth'
dt_pat  <- 'https://docs.google.com/uc?id=%s&export=download'
dt_tab  <- data.frame(openxlsx::read.xlsx(sprintf(dt_pat, dt_fil,download.method = 'curl')))
dt_tab$P3013_laben <- NULL
dt_tab$P3013_cat_laben <- NULL

dt_pro <- openxlsx::read.xlsx('L:/01.ecsc2020/01.tablas/lab_tipologia.xlsx')
dt_tab <- merge(dt_tab,dt_pro[c('P3013_lab','P3013_cat_labenS','P3013_laben')], 
                all.x = T, by = 'P3013_lab')

table(dt_tab$P3013_lab %in% dt_pro$P3013_lab )
table(is.na(dt_tab$P3013_cat_labenS))
dt_tab <- merge(dt_tab,a4_labP1672, all.x = T, by = 'P1672')
dt_tab <- merge(dt_tab,a4_labP1675, all.x = T, by = 'P1675')
dt_tab <- merge(dt_tab,a4_labP1676, all.x = T, by = 'P1676')
dt_tab <- merge(dt_tab,a4_labP1676S3, all.x = T, by = 'P1676S3')
dt_tab <- merge(dt_tab,a4_labP1677, all.x = T, by = 'P1677')
dt_tab <- merge(dt_tab,a4_labP1678, all.x = T, by = 'P1678')
dt_tab <- merge(dt_tab,a4_labP1679, all.x = T, by = 'P1679')
dt_tab <- merge(dt_tab,a4_labP1681, all.x = T, by = 'P1681')
dt_tab <- merge(dt_tab,a4_labP1682, all.x = T, by = 'P1682')
dt_tab <- merge(dt_tab,a4_labP1683, all.x = T, by = 'P1683')
dt_tab <- merge(dt_tab,a4_labP1674, all.x = T, by = 'P1674')

dt_tab$sexo <- 'Male'
dt_tab$sexo[dt_tab$P220 == 2] <- 'Female'

dt_tab$pro <- 0 # universo de problemas caracterizados - 8.204.419 - cl+cc
dt_tab$njg <- 0 # njg caracterizadas con registro de solución - 6.499.532 - cl+cc
dt_tab$prc <- 0 # problemas caracterizados con registro de solución - 8.099.476 - cl+cc

# problemas caracterizados ciclo largo - 7.435.984 - cl+cc
# universo de problemas declarados - 8.266.991 - cl+cc

dt_tab$njg[dt_tab$P1672 != 2 & dt_tab$P1685 != 9] <- 1
dt_tab$njg[dt_tab$P1685 != 9] <- 1
dt_tab$prc[dt_tab$P1685 != 9] <- 1
dt_tab$pro[dt_tab$P1685 != 9] <- 1

sum(dt_tab$FEX_C[dt_tab$pro == 1])
sum(dt_tab$FEX_C[dt_tab$njg == 1])
sum(dt_tab$FEX_C[dt_tab$prc == 1])

sum(dt_tab$FEX_C[dt_tab$njg == 1])
sum(dt_tab$FEX_C[dt_tab$pro == 1])
sum(dt_tab$FEX_C[dt_tab$prc == 1])
sum(dt_tab$FEX_C[dt_tab$pro == 1 & dt_tab$nj_class2 == 1])
sum(dt_tab$FEX_C[dt_tab$prc == 1 & dt_tab$nj_class2 == 1])


sum(dt_tab$FEX_C[dt_tab$njg == 1])
sum(dt_tab$FEX_C[dt_tab$pro == 1])
sum(dt_tab$FEX_C[dt_tab$prc == 1])
sum(dt_tab$FEX_C[dt_tab$pro == 1 & dt_tab$nj_class2 == 1])
sum(dt_tab$FEX_C[dt_tab$prc == 1 & dt_tab$nj_class2 == 1])

dt_tab$njg_lab <- 'Legal need' 
dt_tab$njg_lab[dt_tab$njg == 0] <- 'Problem'
dt_tab$pro_lab <- 'Problem'

dt_tab$P1685[dt_tab$P1685 == 2] <- 0
dt_tab$problema <- 1


# a.5.Control merging ------------------------------------------------------------------------------

## a.5.1. a2_12 Contribution to security------------------------------------------------------------

a2_12$keyper <- paste0(
  a2_12$DIRECTORIO,
  a2_12$NRO_ENCUESTA,
  a2_12$SECUENCIA_P,
  a2_12$ORDEN
)

table(duplicated(dt_tab$keyper))
table(duplicated(a2_12$keyper))

table(dt_tab$keyper %in% a2_12$keyper)

dt_tab <- merge(dt_tab, a2_12[,c('keyper',
                              'P1182S1',   #Police
                              'P1182S3',   #Major
                              'P1181S1',   #Prosecutor
                              'P1181S2')], #Judges
                by = 'keyper', all.x = TRUE)

table(is.na(dt_tab$P1181S2))

## a.5.2. a2_03 security perception ----------------------------------------------------------------

a2_03$keyper <- paste0(
  a2_03$DIRECTORIO,
  a2_03$NRO_ENCUESTA,
  a2_03$SECUENCIA_P,
  a2_03$ORDEN
)

table(duplicated(dt_tab$keyper))
table(duplicated(a2_03$keyper))

table(dt_tab$keyper %in% a2_03$keyper)

dt_tab <- merge(dt_tab, a2_03[,c('keyper',
                                 'P3105',   #Perception of safety in the neighborhood
                                 'P3106',   #Perception of safety when walking at night
                                 'P3107',   #Perception of safety in the city
                                 'P564')],  #Possibility of being a victim of crimes in the future
                by = 'keyper', all.x = TRUE)

table(is.na(dt_tab$P1181S2))

## a.5.3. a2_05 crime filter -----------------------------------------------------------------------

a2_05$keyper <- paste0(
  a2_05$DIRECTORIO,
  a2_05$NRO_ENCUESTA,
  a2_05$SECUENCIA_P,
  a2_05$ORDEN
)

table(duplicated(dt_tab$keyper))
table(duplicated(a2_05$keyper))

table(dt_tab$keyper %in% a2_05$keyper)

dt_tab <- merge(dt_tab, a2_05[,c('keyper',
                                 'P525',   #Victim of theft in 2020
                                 'P1343'   #Victim of theft 2019
                                 )],
                by = 'keyper', all.x = TRUE)

table(is.na(dt_tab$P525))
table((dt_tab$P525)); table((dt_tab$P1343))

dt_tab$victim_theft <- 0

dt_tab$victim_theft[which(dt_tab$P525 == 1 | dt_tab$P1343 == 1 )] <- 1
table(dt_tab$nj_1_count,dt_tab$victim_theft)

## a.5.4. a2_05 victims of crime -------------------------------------------------------------------


a2_06$keyper <- paste0(
  a2_06$DIRECTORIO,
  a2_06$NRO_ENCUESTA,
  a2_06$SECUENCIA_P,
  a2_06$ORDEN
)

table(duplicated(dt_tab$keyper))
table(duplicated(a2_06$keyper))

table(dt_tab$keyper %in% a2_06$keyper)
table(a2_06$keyper %in% dt_tab$keyper)

dt_tab <- merge(dt_tab, a2_06[,c('keyper',
                                 'P1228'
)],
by = 'keyper', all.x = TRUE)

table(is.na(dt_tab$P1228))
table((dt_tab$P1228))

# --

a2_07$keyper <- paste0(
  a2_07$DIRECTORIO,
  a2_07$NRO_ENCUESTA,
  a2_07$SECUENCIA_P,
  a2_07$ORDEN
)

table(duplicated(dt_tab$keyper))
table(duplicated(a2_07$keyper))

table(dt_tab$keyper %in% a2_07$keyper)
table(a2_07$keyper %in% dt_tab$keyper)

dt_tab <- merge(dt_tab, a2_07[,c('keyper',
                                 'P2073'
)],
by = 'keyper', all.x = TRUE)

table(is.na(dt_tab$P2073))
table((dt_tab$P2073))

# --

a2_08$keyper <- paste0(
  a2_08$DIRECTORIO,
  a2_08$NRO_ENCUESTA,
  a2_08$SECUENCIA_P,
  a2_08$ORDEN
)

table(duplicated(dt_tab$keyper))
table(duplicated(a2_08$keyper))

table(dt_tab$keyper %in% a2_08$keyper)
table(a2_08$keyper %in% dt_tab$keyper)

dt_tab <- merge(dt_tab, a2_08[,c('keyper',
                                 'P1238'
)],
by = 'keyper', all.x = TRUE)

table(is.na(dt_tab$P1238))
table((dt_tab$P1238))

# --


a2_09$keyper <- paste0(
  a2_09$DIRECTORIO,
  a2_09$NRO_ENCUESTA,
  a2_09$SECUENCIA_P,
  a2_09$ORDEN
)

table(duplicated(dt_tab$keyper))
table(duplicated(a2_09$keyper))

table(dt_tab$keyper %in% a2_09$keyper)

dt_tab <- merge(dt_tab, a2_09[,c('keyper',
                                 'P1324',    # Was the theft reported ?
                                 'P1114',    # Main reason to report the theft
                                 'P1113'     # Main reason to not report the theft
)],
by = 'keyper', all.x = TRUE)

table(is.na(dt_tab$P1324))
table((dt_tab$P1324)); table((dt_tab$P1114)); table((dt_tab$P1113))

# --

a2_11$keyper <- paste0(
  a2_11$DIRECTORIO,
  a2_11$NRO_ENCUESTA,
  a2_11$SECUENCIA_P,
  a2_11$ORDEN
)

table(duplicated(dt_tab$keyper))
table(duplicated(a2_11$keyper))

table(dt_tab$keyper %in% a2_11$keyper)
table(a2_11$keyper %in% dt_tab$keyper)

dt_tab <- merge(dt_tab, a2_11[,c('keyper',
                                 'P1187'
)],
by = 'keyper', all.x = TRUE)

table(is.na(dt_tab$P1187))
table((dt_tab$P1187))

# --

##  a.5.5. poberty and population ------------------------------------------------------------------
table( a3_ipm$mpio_ccnct %in% dt_tab$DEPMUNI)

dt_tab  <- merge( dt_tab, a3_ipm, all.x = TRUE, by.x = 'DEPMUNI', by.y = 'mpio_ccnct')
dt_tab  <- merge( dt_tab, a3_pob[a3_pob$ÁREA.GEOGRÁFICA == 'Total' & a3_pob$AÑO == '2020',], 
                 all.x = TRUE, by.x = 'DEPMUNI', by.y = 'DPMP')

table(dt_tab$DEPMUNI, dt_tab$mpm)
table(dt_tab$DEPMUNI,dt_tab$MPIO)

##  a.5.6. subjective wellbeing  -------------------------------------------------------------------

a3_wb$DEPMUNI <- as.numeric(a3_wb$DEPMUNI)
dt_tab  <- merge( dt_tab, a3_wb , all.x = TRUE, by = 'DEPMUNI')

table(dt_tab$DEPMUNI, dt_tab$ls_3)

##  a.5.7. Crime records  --------------------------------------------------------------------------

a4_wb$DEPMUNI <- as.numeric(a4_wb$DEPMUNI)
table(a4_wb$DEPMUNI %in% dt_tab$DEPMUNI)
dt_tab  <- merge( dt_tab, a4_wb , all.x = TRUE, by = 'DEPMUNI')

table(dt_tab$DEPMUNI, dt_tab$theft_20182020)


##  a.5.8. Conciliation records  -------------------------------------------------------------------

a5_wb$DEPMUNI <- as.numeric(a5_wb$DEPMUNI)
table(a5_wb$DEPMUNI %in% dt_tab$DEPMUNI)
dt_tab  <- merge( dt_tab, a5_wb , all.x = TRUE, by = 'DEPMUNI')

table(dt_tab$DEPMUNI, dt_tab$conc_tot_20182020)

##  a.5.9. Justice institutions records  -----------------------------------------------------------

a6_wb$DEPMUNI <- as.numeric(a6_wb$DEPMUNI)
table(a6_wb$DEPMUNI %in% dt_tab$DEPMUNI)
dt_tab  <- merge( dt_tab, a6_wb , all.x = TRUE, by = 'DEPMUNI')

table(dt_tab$DEPMUNI, dt_tab$inst_perc10000)

# b. subset ----------------------------------------------------------------------------------------
getwd()

b1_varlist <- openxlsx::read.xlsx('L:/99.LSE studies/99.Dissertation/01.data/01.varlist.xlsx')
names(dt_tab) <- tolower(names(dt_tab))

b1_varlist$var %in% names(dt_tab)
dt_tabs1 <- dt_tab[b1_varlist$var]

names(dt_tabs1)[names(dt_tabs1) == 'total.hombres'] <- 'total_male'
names(dt_tabs1)[names(dt_tabs1) == 'total.mujeres'] <- 'total_female'


var_labels <- c(
fex_c="expansion factor",
depmuni="municipality geo code",
mpio="municipality label",
keyhog="household key",
keyper="person key",
p1988="electricity service in the house",
p1987="type of housing",
p1990="number of households in the home",
p1989="housing class",
p5785="age",
p5501="kinship with the head of household",
p1365="use of time",
p1363="time of day - activity",
p220="sex",
p3105="perception of safety in the neighborhood",
p3106="perception of safety when walking at night",
p3107="perception of safety in the city",
p564="possibility of being a victim of crimes in the future",
p1182s1="contribution to police security",
p1181s2="contribution to the safety of judges",
p1182s3="contribution to the safety of the mayor's office",
p1181s1="contribution to the security of the public prosecutor's office",
p6210="educational level",
p1366="marital status",
keyprob="problem key",
p3013="problem typology",
p3013_laben="problem typology label",
p3013_cat_labens="problem category label",
nj_impacto="problem impact",
nj_mes="problem month of occurrence",
nj_ano="problem year of occurrence",
nj_1_count="count of problem per person",
nj_2_count="count of effective problems",
p1672="action to face the legal need",
p1672_laben="action to face the legal need eng",
p1675="institutional path reasons",
p1676s3="satisfaction from institutional path",
p1676="result from institutional path",
p1674="last visited institution in institutional path",
p1679="individual path reasons",
p1680="individual path outcome",
p1681="reasons for violent path",
p1682="reasons for illegal path",
p1683="reasons for inaction",
p1684="institutional and individual outcome fulfilment ",
p1685="problem status (solved, unsolved)",
p1686="time for solution",
p1687="would you take the same path again",
p1688="reasons for not taking the same path",
ins_count="institutional count in the institutional path",
menor="dummy for presence of under aged kids in  the household",
menor_count="number of underage kids in the household",
nj_class1="long survey cycle",
nj_class2="short survey cycle",
p1988s1 ='public utilities stratum',
ww_1="1 on a scale of 1 to 5, how worthwhile would you say the things you do in your life are? (where 1 means not worth it and 5 means totally worth it)",
ww_2="2 on a scale of 1 to 5, how worthwhile would you say the things you do in your life are? (where 1 means not worth it and 5 means totally worth it)",
ww_3="3 on a scale of 1 to 5, how worthwhile would you say the things you do in your life are? (where 1 means not worth it and 5 means totally worth it)",
ww_4="4 on a scale of 1 to 5, how worthwhile would you say the things you do in your life are? (where 1 means not worth it and 5 means totally worth it)",
ww_5="5 on a scale of 1 to 5, how worthwhile would you say the things you do in your life are? (where 1 means not worth it and 5 means totally worth it)",
wb_total_pob="total population who answer the wb questions",
fe_worried="during the last 7 days you have felt worried",
fe_tired="during the last 7 days you have felt tired",
fe_bad_mood="during the last 7 days you have felt a bad mood",
fe_loneliness="during the last 7 days you have felt lonliness",
sfe_adness="during the last 7 days you have felt sadness",
fe_head_stomach_ache="during the last 7 days you have felt headache stomachache",
fe_cant_sleep="during the last 7 days you have not be able to sleep",
fe_high_heart_rate="during the last 7 days you have felt a high heart rate",
fe_no_positive_feelings="during the last 7 days you have felt no possitive feelings",
fe_none="during the last 7 days you have felt none of the above",
ls_1="1 on a scale of 1 to 5, where 1 is dissatisfied and 5 is satisfied how satisfied do you feel with: life in general",
ls_2="2 on a scale of 1 to 5, where 1 is dissatisfied and 5 is satisfied how satisfied do you feel with: life in general",
ls_3="3 on a scale of 1 to 5, where 1 is dissatisfied and 5 is satisfied how satisfied do you feel with: life in general",
ls_4="4 on a scale of 1 to 5, where 1 is dissatisfied and 5 is satisfied how satisfied do you feel with: life in general",
ls_5="5 on a scale of 1 to 5, where 1 is dissatisfied and 5 is satisfied how satisfied do you feel with: life in general",
hombres_mayor_18="total adult age female population",
mujeres_mayor_18="total adult age male population",
total_male="total male population",
total_female="total female population",
total="total population",
mpm="measure of multidimensional poverty of census source in the total of the municipality",
mpm_cab="measure of multidimensional poverty of census source in the municipal capitals",
mpm_cprd="measure of multidimensional poverty from census source in population centers and dispersed rural areas",
afbmo_tot="indicator of illiteracy in the total municipality",
afbmo_cab="indicator of illiteracy in municipal capitals",
afbmo_cprd="indicator of illiteracy in population centres and dispersed rural areas",
ble_tot="indicator of low educational achievement in the total of the municipality",
ble_cab="indicator of low educational attainment in municipal capitals",
ble_cprd="indicator of low educational attainment in population centers and dispersed rural areas",
bscpi_tot="indicator of barriers to early childhood care services in the total municipality",
bscpi_cab="indicator of barriers to early childhood care services in municipal capitals",
bscpi_cprd="indicator of barriers to early childhood care services in population centers and dispersed rural areas",
bass_tot="indicator of barriers to access to health services in the total municipality",
bass_cab="indicator of barriers to access to health services in municipal capitals",
bass_cprd="indicator of barriers to access to health services in populated centers and dispersed rural areas",
tde_tot="indicator of economic dependency rate in the total municipality",
tde_cab="indicator of economic dependence rate in the municipal capitals",
tde_cprd="indicator of economic dependence rate in populated centers and dispersed rural areas",
hc_tot="indicator of critical overcrowding in the total municipality",
hc_cab="indicator of critical overcrowding in municipal capitals",
hc_cprd="indicator of critical overcrowding in population centers and dispersed rural areas",
iee_tot="indicator of inadequate disposal of excreta in the total of the municipality",
iee_cab="indicator of inadequate disposal of excreta in municipal capitals",
iee_cprd="indicator of inadequate disposal of excreta in populated centers and dispersed rural areas",
ie_tot="indicator of school absence in the total of the municipality",
ie_cab="indicator of school absence in the municipal capitals",
ie_cprd="indicator of school absence in populated centers and dispersed rural areas",
mipe_tot="indicator of inadequate material of exterior walls in the total of the municipality",
mipe_cab="indicator of inadequate material of exterior walls in the municipal capitals",
mipe_cprd="indicator of inadequate material of exterior walls in populated centers and dispersed rural",
mip_tot="indicator of inadequate material of exterior floors in the total of the municipality",
mip_cab="indicator of inadequate material of exterior floors in the municipal capitals",
mip_cprd="indicator of inadequate material of exterior floors in population centers and dispersed rural",
re_tot="indicator of school lag in the total of the municipality",
re_cab="indicator of school lag in the municipal capitals",
re_cprd="indicator of school lag in populated centers and dispersed rural areas",
safam_tot="indicator of no access to improved water source in the total municipality",
safam_cab="indicator of no access to improved water source in municipal capitals",
safam_cprd="indicator of no access to improved water source in populated centers and dispersed rural areas",
sas_tot="indicator of no health insurance in the total of the municipality",
sas_cab="indicator of no health insurance in the municipal capitals",
sas_cprd="indicator of no health insurance in populated centers and dispersed rural areas",
trinf_tot="indicator of child labor in the total of the municipality",
trinf_cab="indicator of child labour in municipal capitals",
trinf_cprd="indicator of child labour in populated centres and dispersed rural areas",
triml_tot="indicator of informal work in the total municipality",
triml_cab="indicator of informal work in municipal capitals",
triml_cprd="indicator of informal work in population centres and dispersed rural areas",
fviolence_2018='family violence 2018',
hom_2018='homicides 2018',
theft_2018='theft 2018',
fviolence_2019='family violence 2019',
hom_2019='homicides 2019',
theft_2019='theft 2019',
fviolence_2020='family violence 2020',
hom_2020='homicides 2020',
theft_2020='theft 2020',
fviolence_20182020='family violence 2018 2020',
hom_20182020='homicides 2018 2020',
theft_20182020='theft 2018 2020',
conc_sol2018='conciliation request 2018',
conc_tot_2018='total effective conciliations 2018',
conc_sol2019='conciliation request 2019',
conc_tot_2019='total effective conciliations 2019',
conc_sol2020='conciliation request 2020',
conc_tot_2020='total effective conciliations 2020',
conc_sol20182020='conciliation request 2018 2020',
conc_tot_20182020='total effective conciliations 2018 2020',
justice_inst='number of jsutice institutions',
inst_perc10000='justice institutions per 10000 inhabitants',
p1324 = 'Was the theft reported',
p1114 = 'Main reason to report the theft',
p1113 = 'Main reason to not report the theft',
p525 = 'Victim of theft in 2020',
p1343 = 'Victim of theft 2019',
victim_theft= 'Victim of theft 2019 2020'

)



label(dt_tabs1) = as.list(var_labels[match(names(dt_tabs1), names(var_labels))])
haven::write_dta(dt_tabs1, "dt_tabs3.dta")

