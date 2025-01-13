####################################
#                                  #
#   GHG-Rechner für Rapsversuche   #
#          02.10.2022              #
#                                  #
####################################

# Version des Abschlussberichts FNR
#
# Standardmethode für Ernteresiduen ist jetzt ResidueCalc="BioGrace"!!!
#
# Ertrag in kg/ha angeben!!! (vorher: dt/ha)
#
# Quotienten feur Ernteresiduen komplett selbst
# berechnet aufgrund Fehlern in der Mittelwertbildung
# im Chalon Datensatz
#
# IncludeDrying nun standardmäßig = FALSE!!!

### eec nach BG II Version 4a Februar 2021

### for the following calculations use skript -Biograce_Beispiel: (X:\rabah_nasser\R_skripts\Biograce_Beispiel)

#' Title GHGCalculator_OSR_4_1_REDII
#'
#' @param Ertrag
#' seed yield in (kg/ha) give as fresh dry matter
#' @param Treatment
#' Optional Vector with Treatment/Variant-Codes
#' @param Feuchte
#' water content of OSR seed given as (\%), default value is 9\%
#' @param Oil_9
#' Oil content of OSR seed given as (\%), at 9\% moisture, default is 42.6
#' @param Nmineral
#' mineral N-fertilisation in (kg N/ha), default value is 142 kg N/ha
#' @param SimNLeach
#' Optional direct quantification of N-Leaching in (kg N/ha), by using own estimates or from simulation modelling
#' @param SimN2ON
#' Optional direct quantification of direct N2O-N Emissions in (kg N/ha), by using own estimates or from simulation modelling
#' @param SimResidue
#'  Optional direct quantification of N-amount in Crop residues in (kg N/ha), by using own estimates or from simulation modelling
#' @param sim
#' logical (vector) for using own estimates/simulated input values
#' @param Norgan
#' organic N fertilization in (kg/ha)
#' @param P2O5
#' Optional fertilisation rate of phosphorous in (kg P2O5/ha), if not given, fertilisation is assumed to replace P offtake by seeds
#' @param K2O
#' Optional fertilisation rate of potassium in (kg K2O/ha), if not given fertilisation is assumed to replace K offtake by seeds
#' @param CaCO3
#' Optional rate of liming in (kg CaCO3/ha)
#' @param PSM
#' Input of plant protection products in (kg/ha)
#' @param Saatgut
#' seed input (kg/ha)
#' @param Diesel
#' Diesel use for field operations default value is 83.3 l/ha
#' @param Field.acidification
#' needed??
#' @param ep
#' emissions from production of biodiesel (g CO2eq/MJ), default is 11.7 g CO2eq/MJ, (typical value - REDII)
#' @param etd
#' emissions for transport in the production chain in (g CO2eq/MJ)
#' @param ResidueCalc
#' Option for calculation of N from crop residues choices: "Chalon", "IPCC", "DueV", "BioGrace", "SimValue"
#' @param N2OCalcMethod
#' Tier 1 uses aggregated emission factors for disaggretated factors use option "custom"
#' @param IncludeOrganic
#' inclusion of organic fertiliser N in emission calculation (T/F, yes, no)
#' @param IncludeResidues
#' inclusion of crop residues N in emission calculation  (T/F, yes, no)
#' @param IncludeSynthetic
#' inclusion of synthetic N in emission calculation  (T/F, yes, no)
#' @param N2OSimMethod
#' Options for calculation of direct N2O-emissions "IPCC", # "HUME ,GNOC, DNDC und MODE" SimN2ON nicht 0
#' @param IncludeDrying
#' include emissions due to seed drying
#' @param IncludeVolatil
#' option (T/F) for inclusion (exclusion) of gaseous emission, default is inclusion T
#' @param IncludeLeaching
#' option (T/F) for inclusion (exclusion) of leaching based emission, default is inclusion T
#' @param OtherEm_ha
#' other emission in kg CO2eq/ha
#' @param OtherEm_MJ
#' other emissions in MJ
#' @param Dis.EF1.F_SN
#' Synthetic fertilizer inputs in wet climates IPCC(2019) 0.019 is default value
#' @param Dis.EF1.F_ON
#' Other N inputs in wet climates IPCC (2019)  0.006 is default
#' @param Dis.EF1.F_CR
#' disaggretated emission factor for N input from crop residues, 0.006 is default
#' @param Dis.IPCC.EF4
#'  emission factor for N2O emissions from atmospheric deposition of N on soils and water surfaces
#' @param Dis.CAS.Frac_GASF
#' disaggregated ammonia emission factor (Volatilization from synthetic fertilizer), 0.05 is default (ammonia-nitrate)
#' @param Frac_NLeach
#' Leaching fraction of N-input, default value is 0.24 according to IPCC 2019
#' @param EF.SN.Prod
#' Emission for production of synthetic N fertilizer in kg CO2eq/kg N, default is 3.469 for calcium-ammonium nitrate from additinal standard values of BioGraceII
#' @param Show
#' Options for output
#'
#' @return data frame with outputs according to the Show parameter
#' @export
#'
#' @examples

GHGCalculator_OSR_4_1_REDII <- function(Ertrag=3503,
                                   Treatment,
                                   Feuchte=9.0,
                                   Oil_9=42.6,
                                   Nmineral=142,
                                   SimNLeach=0,
                                   SimN2ON=0,
                                   SimResidue=0,
                                   sim=FALSE,
                             Norgan=0,
                             P2O5=NULL,
                             K2O=NULL,
                             CaCO3=312.97984872041,
                           #  Dataset=NULL,
                             PSM=6.609977798,
                             Saatgut=28,
                             Diesel=83.3,
                             Field.acidification=NULL,
                             ep=11.7, ##(typical value - REDII)
                             etd=1.8,
                             ResidueCalc="BioGrace",
                             N2OCalcMethod="Tier1", # Tier 1 uses aggregated emission factors for disaggretated factors use option "custom"
                             IncludeOrganic=TRUE,
                             IncludeResidues=TRUE,
                             IncludeSynthetic=TRUE,
                             N2OSimMethod= "IPCC", # "HUME ,GNOC, DNDC und MODE" SimN2ON nicht 0
                             IncludeDrying=FALSE,
                             IncludeVolatil=TRUE,
                             IncludeLeaching=TRUE,
                             OtherEm_ha=0, OtherEm_MJ=0,
                             Dis.EF1.F_SN = 0.016,                                     #Synthetic fertilizer inputs in wet climates IPCC(2019)
                             Dis.EF1.F_ON = 0.006,                                     #EF1 for other N inputs in wet climates IPCC (2019)
                             Dis.EF1.F_CR = 0.006,                                     #EF1 for Crop Residue N inputs in wet climates IPCC (2019)
                             Dis.IPCC.EF4 = 0.014,                                     #wet climates IPCC (2019)
                             Dis.CAS.Frac_GASF = 0.05,
                             Frac_NLeach = 0.24,
                             EF.SN.Prod = 3.469,
                             Show=list("complete", "eec", "eec.ha", "eec.MJ",
                                       "N2O", "N2O.detail.ha", "N2O.detail.MJ",
                                       "N2O.detail.ha.more", "N2O.detail.MJ.more",
                                       "compare", "internal")){


  # Hilfsfunktion zum Auslesen der übergebenen
  # Parameter. Recycelt einen numerischen Vektor
  # bis länge eines Zielvektors oder einer Datenfeld-
  # spalte erreicht wurde
  Recycle <- function(From, To){
    Rec <- To
    if(is.numeric(To)){
      Rec[] <- rep(From, length(To) %/% length(From) + 1)[1:length(To)]
    }else{
      if(is.data.frame(To)){
        Rec[] <- rep(From, nrow(To) %/% length(From) + 1)[1:nrow(To)]
      }else{ Rec[] <- NA
      }
    }
    Rec
  }

  ValidArg.Show <- list("complete", "eec", "eec.ha",
                        "eec.MJ", "N2O", "N2O.detail.ha", "N2O.detail.MJ",
                        "N2O.detail.ha.more", "N2O.detail.MJ.more",
                        "compare", "internal", "minimal")

  # Validating the input for N2OCalcMethod
  ValidArg.N2O <- list("Tier1", "custom")
  stopifnot(N2OCalcMethod %in% ValidArg.N2O)

  ##### constants       ####
  #####  Reference-/ standard values ####

  Durchschnitt.Ertrag.DE <- 3503
  #German Federal Statistical Office-Destatis. German wheat production area and annual wheat yield. Available
  # online: https://www.destatis.de/DE/ZahlenFakten/Wirtschaftsbereiche/LandForstwirtschaftFischerei/
  #   FeldfruechteGruenland/Tabellen/FeldfruechteZeitreihe.html (accessed on 15.11.2021)

  Ref.RED.ef.Diesel <- 94 #g CO2eq/MJ (Edwards et al., 2019)
  Ref.RED.ef.CO2 <- 1
#  Ref.RED.ef.N2O <- 298
  # emission factor for N2O according to IPCC 2019 Tier 1
  Ref.RED.ef.N2O <- 274
#  Ref.RED.ef.CH4 <- 25
  # emission factor for CH4 according to IPCC 2019 Tier 1
  Ref.RED.ef.CH4 <- 27

  # Wenn nicht anders angegeben
  # alle Werte in kg CO2eq/kg nutrient
  # Use parameter value for synthetic N fertilizer
  Ref.BG.ef.N <- EF.SN.Prod
  # Emission factors according to BioGrace II [ g CO2eq/kg nutrient]
  Ref.BG.ef.P2O5  <- 0.5416667
  Ref.BG.ef.K2O   <- 0.4166700
  Ref.BG.ef.CaCO3 <- 0.03906964
  Ref.BG.ef.PSM   <- 12.0106546
  Ref.BG.ef.Saatgut <- 0.7565014
  Ref.BG.ef.Electricity <- 0.17031314   #BG II additional Standardvalues --> IFEU calculations ?? per MJ Energy?
  Ref.BG.ef.NaturalGas <- 0.066         #JRC Report EUR 28349 (2019)
  Ref.BG.ef.Diesel.MJ <- 95.1 / 1000
  Ref.BG.Diesel.MJ_kg <- 43.1
  Ref.BG.Diesel.kg_l <- 0.832
  Ref.BG.Diesel.MJ_l <- Ref.BG.Diesel.kg_l * Ref.BG.Diesel.MJ_kg
  Ref.BG.ef.Diesel.l <- Ref.BG.ef.Diesel.MJ * Ref.BG.Diesel.MJ_l
  Ref.BG.ef.Diesel.use.CH4.N2O <- 0.000973294663573
  Ref.JRC.FA.lime <- 0.079                                                #JRC Report EUR 28349 (2019)
  Ref.JRC.FA.N <- 0.798                                                   #JRC Report EUR 28349 (2019)
  Ref.FA.N.EPA <- 0.386
  Ref.BG.conversion <- 0.578448289873776
#  Ref.BG.allocation <- 0.585891295825312
## Calculation of allocation factor according to input oil concentration (@9% moisture)
  ## constants from JRC Report EUR 28349 (2019)
## Interaction of oil and protein concentration is considered according to an empirical regression

  LHV_Oil <- (Oil_9/0.91*37)/100 # contribution of oil to LHV of osr seed
  #  Per percent oil increase protein decreases at about 0.7%
  Perc_Protein <- 18.3-(Oil_9/0.91-46.8)*0.7 #
  LHV_Protein <- Perc_Protein/100*24.5 # fractional part of LHV seed from protein
  SumOilProtein <- Oil_9/0.91+Perc_Protein # sum of oil and protein fraction in (%)
  RestFraction <- 100-SumOilProtein # Restfraction
  carbo_rest_fraction <- 19/(19+11.8+4.1) # fraction of carbohydrates in rest fraction of seed (after substraction of oil%protein)
  fibre_rest_fraction <- 11.8/(19+11.8+4.1) # fraction of carbohydrates in rest fraction of seed (after substraction of oil%protein)
  carbo_fraction <- carbo_rest_fraction*RestFraction
  Fibre_fraction <- fibre_rest_fraction*RestFraction
  LHV_carbo <- carbo_fraction*15.88/100
  LHV_fibre <- Fibre_fraction*18.27/100
#  Ref.BG.Raps.MJ_kg <- 26.9755818635146                                   # reiner Energiegehalt, keine Konversion; neu nach BG II
  Ref.BG.Raps.MJ_kg <- LHV_Oil+LHV_Protein+LHV_carbo+LHV_fibre                                   # reiner Energiegehalt, keine Konversion; neu nach BG II
# new version hk 29.09.2022 with Energy content calculated from variable oil concentration
  Ref.BG.allocation <- (LHV_Oil)/Ref.BG.Raps.MJ_kg
  Kalkung.kg_MJRaps <- 0.00405

###### Emissionswerte direkte und  indirekte N2O-Emission #####
  # Werte für direkte und indirekte N2O-Emission  -  müssten nach Biograce II über GNOC berechnet werden
  # nach IPCC 2019 Tier 1
  Ref.IPCC.ef.N2ON <- 44 / 28
  # Ref.IPCC.EF1 <- 0.01
  if(N2OCalcMethod=="Tier1"){
    Ref.IPCC.EF1.F_SN <- 0.01
    Ref.IPCC.EF1.F_ON <- 0.01
    Ref.IPCC.EF1.F_CR <- 0.01
  }else{
    Ref.IPCC.EF1.F_SN <- Dis.EF1.F_SN
    Ref.IPCC.EF1.F_ON <- Dis.EF1.F_ON
    Ref.IPCC.EF1.F_CR <- Dis.EF1.F_CR
  }


#### Definition of parameters for calculation of crop residue DM ####
  Ref.IPCC.AG_DM.Slope <- 1.5  #GNOC 2014
  Ref.IPCC.AG_DM.Intercept <- 0.0 #GNOC 2014
  #Ref.IPCC.R_AG <- 0.19 # bisher unbekannt
  Ref.IPCC.R_S <- 0.19   # GNOC 2014
  Ref.IPCC.N_AG <- 0.011 #GNOC 2014
  Ref.IPCC.N_BG <- 0.017 #GNOC 2014

  #Ref.IPCC.Frac_GASF <- 0.11
  if(N2OCalcMethod=="Tier1"){
    Ref.IPCC.Frac_GASF <- 0.11
  }else{
    Ref.IPCC.Frac_GASF <- Dis.CAS.Frac_GASF
  }

  Ref.IPCC.Frac_GASM <- 0.21
  #Ref.IPCC.EF4 <- 0.010
  if(N2OCalcMethod=="Tier1"){
    Ref.IPCC.EF4 <- 0.010
  }else{
    Ref.IPCC.EF4 <- Dis.IPCC.EF4
  }

  Ref.IPCC.Frac_LEACH <- Frac_NLeach
  Ref.IPCC.EF5 <- 0.011


##### parameter values for crop residue calculation according to BioGrace #####
  Ref.BG.AG_DM.Slope <- 1.5
  Ref.BG.AG_DM.Intercept <- 0
  Ref.BG.N_AG <- 0.011
  Ref.BG.N_BG <- 0.017
  Ref.BG.R_S <- 0.19

##### Parameter for calculation N in crop residues according to Chalon Data-Base ####
  Ref.INRA.HI <- 0.3173163295859760 # Harvest-Index
  Ref.INRA.Nc.AG <- 0.0147929879957536 # N-Konzentration oberirdische Biomasse insgesamt
  Ref.INRA.Nc.Root <- 0.0087507692307692 # N-Konzentration Wurzeln
  Ref.INRA.Nc.Seed <- 0.0300778485491861 # N-Konzentration Samen
  Ref.INRA.Root_Shoot <- 0.0552916678008547 # Wurzel zu Spross-Verhältnis

##### N in Ernteresiduen nach DueV ####
  Ref.DueV.Nc.Seed <- (3.35 / 100) / 0.91 # Bezug in der DüV ist 9%!!!
  Ref.DueV.HI <- 0.91 / (0.91 + 1.7 * 0.86) # In DüV ist Korn:Stroh gegeben das auf Basis 9% bzw 14%
  Ref.DueV.Nc.AG <- ((4.54 /100 ) / 0.91) / (1 / Ref.DueV.HI)

### KTBL-Werte aus "Faustzahlen f. d. Landwirtschaft" ####
  Ref.KTBL.Entz.P <- 8.0 #(Element; Basis 9% Feuchte! kg/t)
  Ref.KTBL.Entz.K <- 8.3 #(Element; Basis 9% Feuchte! kg/t)
  Ref.KTBL.dry.Therm_MJ_kgH2O <- 5 #MJ/kg H2O
  Ref.KTBL.dry.Ele.kWh_t <-4 #kWh/t Gut(feucht)
  Ref.KTBL.dry.Ele.MJ_t <- Ref.KTBL.dry.Ele.kWh_t * 3.6 #MJ/t Gut(feucht)

#### parameters for converting nutrient from oxide to element #####
  # molecular weights
  Const.m.P <- 30.974
  Const.m.K <- 39.098
  Const.m.O <- 15.999
  Const.m.P2O5 <- 2 * Const.m.P + 5 * Const.m.O
  Const.m.K2O <- 2 * Const.m.K + Const.m.O
  Const.CV.PtoP2O5 <- Const.m.P2O5 / (2 * Const.m.P)
  Const.CV.KtoK2O <- Const.m.K2O / (2 * Const.m.K)


#### Prüfung/Umrechnung Eingabeparameter ####
    # Zuweisung der übergebenen Eingangsparameter
  # Für spätere Ausgabe müssen alle Parameter
  # als Vektoren der gleichen Länge wie der
  # Ertragsvektor vorliegen.

  # Ertrag <- as.numeric(unlist(Ertrag))
  # Nmineral <- as.numeric(unlist(Nmineral))
  #
  # SimResidue <- as.numeric(unlist(SimResidue))
  # Nmineral <- as.numeric(unlist(Nmineral))
  # SimNLeach <- as.numeric(unlist(SimNLeach))
  # SimN2ON <- as.numeric(unlist(SimN2ON))
  #

  #  Dataset als Data Frame (Falls liste o.ä.)
#  if(!is.null(Dataset)){Dataset <- as.data.frame(Dataset)}

  ### Ertrag ###
  # Ertrag
  if(is.null(Ertrag)) {
    print(paste("Fehler in 'Ertrag':",
                "Parameter nicht optional"))
  }

  if(is.numeric(Ertrag)) {
    Out.Ertrag.Feucht <- Ertrag
    Out.Ertrag.Feucht <- as.data.frame(Out.Ertrag.Feucht)
  }

#  if(!is.null(Ertrag)&!is.numeric(Ertrag)){
#    if(!is.null(Dataset)& Ertrag %in% names (Dataset)){
#      Out.Ertrag.Feucht <- Dataset[Ertrag]
#    }else{ print(paste("Fehler in 'Ertrag':",
#                       "Erwartet wird Spaltenbezeichner für 'Dataset'",
#                       "oder ein numerischer Wert/Vektor"))
#    }
#  }
  names(Out.Ertrag.Feucht) <- "Out.Ertrag.Feucht"


  # Feuchte
  if(is.numeric(Feuchte)) {
    Out.Feuchte <- Out.Ertrag.Feucht * 0
    Out.Feuchte[] <- Recycle(Feuchte / 100, Out.Feuchte)
  }


#  if(!is.null(Feuchte) & !is.numeric(Feuchte)){
#    if(!is.null(Dataset)&Feuchte %in% names (Dataset)){
#      Out.Feuchte <- Dataset[Feuchte] /100
#    }else{ print(paste("Fehler in 'Feuchte':",
#                       "Erwartet wird Spaltenbezeichner für 'Dataset'",
#                       "oder ein numerischer Wert/Vektor"))
#    }
#  }
  names(Out.Feuchte) <- "Out.Feuchte"

  # Ertrag als DM
  Out.Ertrag.DM <- Out.Ertrag.Feucht * (1 - Out.Feuchte)
  names(Out.Ertrag.DM) <- "Out.Ertrag.DM"

  # Ertrag standardisiert auf 9% Feuchte
  Out.Ertrag.9prz <- Out.Ertrag.DM / 0.91
  names(Out.Ertrag.9prz) <- "Out.Ertrag.9prz"

  ## Treatment name
#  if(!is.null(Dataset)){
#    if(!is.null(Treatment) & "Treatment" %in% names (Dataset)) {
#      Treatment<- Dataset[Treatment]

#    } else {

#      print(paste("Fehler in 'Treatment':",
#                  "Erwartet wird Spaltenbezeichner feur 'Dataset'"))

#    }
#
#
#  } else {
#    if(!is.null(Treatment)){ Treatment <- Treatment
#    } else {
#      Treatment <- "NA"
#    }
#  }

  ###

  if (!is.null(Treatment)){
  if (length(Treatment)!= length(Out.Ertrag.Feucht)){
    Treatment <- rep(Treatment, length(Out.Ertrag.Feucht))
  }

  Treatment <- as.data.frame(Treatment)
  names(Treatment) <- "Treatment"
  }

#### Berechnung Trocknung #####
##### Notwendige zu verdampfende Wassermenge bis 9% H2O ####

  Int.Dry.H2O <- Out.Ertrag.Feucht * 0
  for (i in 1:nrow(Out.Feuchte)){
    if(Out.Feuchte[i,] > 0.09){
      Int.Dry.H2O[i,] <- Out.Ertrag.Feucht[i,] * (Out.Feuchte[i,] - (1 - Out.Feuchte[i,]) / 0.91 * 0.09)
    }else{ Int.Dry.H2O[i,] <- 0
    }
  }
  names(Int.Dry.H2O) <- "Int.Dry.H2O"

##### Wärmebedarf Trocknung MJ/ha ####
  if(IncludeDrying){
    In.Dry.Therm_MJ <- Int.Dry.H2O * Ref.KTBL.dry.Therm_MJ_kgH2O
  }else{In.Dry.Therm_MJ <- Int.Dry.H2O * 0}
  names(In.Dry.Therm_MJ) <- "In.Dry.Therm_MJ"

##### Bedarf elektr. Energie MJ/ha ####
  if(IncludeDrying){
    In.Dry.Electric <- Out.Ertrag.Feucht / 1000 * Ref.KTBL.dry.Ele.MJ_t
  }else{In.Dry.Electric <- Out.Ertrag.Feucht * 0}
  names(In.Dry.Electric) <- "In.Dry.Electric"



##### Düngung ######
###### Mineralische Düngung, Vorbereitung Ausgabevariablen ####
  if(is.numeric(Nmineral)) {
    In.F_SN <- Out.Ertrag.Feucht * 0
    In.F_SN[] <- Recycle(Nmineral, In.F_SN)
  }

#  if(!is.null(Nmineral) & !is.numeric(Nmineral)){
#    if(!is.null(Dataset) & Nmineral %in% names (Dataset)){
#      In.F_SN <- Dataset[Nmineral]
#    }else{ print(paste("Fehler in 'Nmineral':",
#                       "Erwartet wird Spaltenbezeichner für 'Dataset'",
#                       "oder ein numerischer Wert/Vektor"))
#    }
#  }
  names(In.F_SN) <- "In.F_SN"

###### Organische Düngung ####
  if(is.numeric(Norgan)) {
    In.F_ON <- Out.Ertrag.Feucht * 0
    In.F_ON[] <- Recycle(Norgan, In.F_ON)
  }

#  if(!is.null(Norgan) & !is.numeric(Norgan)){
#    if(!is.null(Dataset) & Norgan %in% names (Dataset)){
#      In.F_ON <- Dataset[Norgan]
#    }else{ print(paste("Fehler in 'Norgan':",
#                       "Erwartet wird Spaltenbezeichner für 'Dataset'",
#                       "oder ein numerischer Wert/Vektor"))
#    }
#  }
  names(In.F_ON) <- "In.F_ON"

pH <- 6.5
###### Kalkung? #####
  if(is.numeric(pH)) {
    pH_in <- Out.Ertrag.Feucht * 0
    pH[] <- Recycle(pH, pH_in)
  }
  #names(Dataset)
#  if(!is.null(pH) & !is.numeric(pH)){
#    if(!is.null(Dataset) & pH %in% names (Dataset)){
#      pH <- Dataset[pH]
#    }else{ print(paste("Fehler in 'pH':",
#                       "Erwartet wird Spaltenbezeichner für 'Dataset'",
#                       "oder ein numerischer Wert/Vektor"))
#    }
#  }
  names(pH) <- "pH"


###### P2O5-Düngung ####
  if(is.null(P2O5)) {
    In.P2O5 <- Out.Ertrag.9prz / 1000 * Ref.KTBL.Entz.P * Const.CV.PtoP2O5
  }
  #In.P2O5 <- 3503/ 1000 * Ref.KTBL.Entz.P * Const.CV.PtoP2O5

  if(is.numeric(P2O5)) {
    In.P2O5 <- Out.Ertrag.Feucht * 0 # Düngung muss als Vektor
    In.P2O5[] <- Recycle(P2O5, In.P2O5) # der selben Länge wie Datensatz vorliegen
  }

#  if(!is.null(P2O5) & !is.numeric(P2O5)){
#    if(!is.null(Dataset) & P2O5 %in% names (Dataset)){
#      In.P2O5 <- Dataset[P2O5]
#    }else{ print(paste("Fehler in 'P2O5':",
#                       "Erwartet wird Spaltenbezeichner für 'Dataset'",
#                       "oder ein numerischer Wert/Vektor"))
#    }
#  }
  names(In.P2O5) <- "In.P2O5"

###### K2O-Düngung #####
  if(is.null(K2O)) {
    In.K2O <- Out.Ertrag.9prz / 1000 * Ref.KTBL.Entz.K * Const.CV.KtoK2O
  }

  if(is.numeric(K2O)) {
    In.K2O <- Out.Ertrag.Feucht * 0 # Düngung muss als Vektor
    In.K2O[] <- Recycle(K2O, In.K2O) # der selben Länge wie Datensatz vorliegen
  }

#  if(!is.null(K2O)&!is.numeric(K2O)){
#    if(!is.null(Dataset)&K2O %in% names (Dataset)){
#      In.K2O <- Dataset[K2O]
#    }else{ print(paste("Fehler in 'K2O':",
#                       "Erwartet wird Spaltenbezeichner für 'Dataset'",
#                       "oder ein numerischer Wert/Vektor"))
#    }
#  }
  names(In.K2O) <- "In.K2O"


##### CaCO3-Kalkung #####
  if(is.null(CaCO3)) {
    In.CaCO3 <- Durchschnitt.Ertrag.DE * (1-Out.Feuchte) * Ref.BG.Raps.MJ_kg * Kalkung.kg_MJRaps
  }

  if(is.numeric(CaCO3)) {
    In.CaCO3 <- Out.Ertrag.Feucht * 0 # Düngung muss als Vektor
    In.CaCO3[] <- Recycle(CaCO3, In.CaCO3) # der selben Länge wie Datensatz vorliegen
  }

#  if(!is.null(CaCO3) & !is.numeric(CaCO3)){
#    if(!is.null(Dataset) & CaCO3 %in% names (Dataset)){
#      In.CaCO3 <- Dataset[CaCO3]
#    }else{ print(paste("Fehler in 'CaCO3':",
#                       "Erwartet wird Spaltenbezeichner für 'Dataset'",
#                       "oder ein numerischer Wert/Vektor"))
#    }
#  }
  names(In.CaCO3) <- "In.CaCO3"


##### Pflanzenschutz, Vorbereitung Ausgabe #####
  if(is.numeric(PSM)) {
    In.PSM <- Out.Ertrag.Feucht * 0
    In.PSM[] <- Recycle(PSM, In.PSM)
  }

#  if(!is.null(PSM) & !is.numeric(PSM)){
#    if(!is.null(Dataset) & PSM %in% names (Dataset)){
#      In.PSM <- Dataset[PSM]
#    }else{ print(paste("Fehler in 'PSM':",
#                       "Erwartet wird Spaltenbezeichner für 'Dataset'",
#                       "oder ein numerischer Wert/Vektor"))
#    }
#  }
  names(In.PSM) <- "In.PSM"

##### Saatgut #####
  if(is.numeric(Saatgut)) {
    In.Saatgut <- Out.Ertrag.Feucht * 0
    In.Saatgut[] <- Recycle(Saatgut, In.Saatgut)
  }

#  if(!is.null(Saatgut) & !is.numeric(Saatgut)){
#    if(!is.null(Dataset) & Saatgut %in% names (Dataset)){
#      In.Saatgut <- Dataset[Saatgut]
#    }else{ print(paste("Fehler in 'Saatgut':",
#                       "Erwartet wird Spaltenbezeichner für 'Dataset'",
#                       "oder ein numerischer Wert/Vektor"))
#    }
#  }
  names(In.Saatgut) <- "In.Saatgut"

##### Diesel #####
  if(is.numeric(Diesel)) {
    In.Diesel <- Out.Ertrag.Feucht * 0
    In.Diesel[] <- Recycle(Diesel, In.Diesel)
  }

#  if(!is.null(Diesel) & !is.numeric(Diesel)){
#    if(!is.null(Dataset) & Diesel %in% names (Dataset)){
#      GHG.Diesel <- Dataset[Diesel]
#    }else{ print(paste("Fehler in 'Diesel':",
#                       "Erwartet wird Spaltenbezeichner für 'Dataset'",
#                       "oder ein numerischer Wert/Vektor"))
#    }
#  }
  names(In.Diesel) <- "In.Diesel"


##### Emissionen - Field.acidification #####

  if(is.numeric(Field.acidification)) {
    In.Field.acidification <- Out.Ertrag.Feucht * 0
    In.Field.acidification[] <- Recycle(Field.acidification, In.Field.acidification)
  }

#  if(!is.null(Field.acidification) & !is.numeric(Field.acidification)){
#    if(!is.null(Dataset) & Field.acidification %in% names (Dataset)){
#      In.Field.acidification <- Dataset[Field.acidification]
#    }else{ print(paste("Fehler in 'Field.acidification':",
#                       "Erwartet wird Spaltenbezeichner fuer 'Dataset'",
#                       "oder ein numerischer Wert/Vektor"))
#    }
#  }
  # if(is.null(Field.acidification)) {
  #  In.Field.acidification <- In.CaCO3 * Ref.JRC.FA.lime + In.F_ON*0.5* Ref.JRC.FA.N - In.F_SN * Ref.JRC.FA.N        #in kg CO2/ha
  # }
  if(is.null(Field.acidification)) {

  #   In.Field.acidification <- ifelse (pH >=6.4, ### JRC depending on pH
  #                                     In.CaCO3[,1] * Ref.JRC.FA.lime - In.F_SN[,1] * Ref.FA.N.EPA, In.CaCO3[,1] * 0.44 - In.F_SN[,1] * Ref.JRC.FA.N)
  #   # str(In.Field.acidification)
  #   In.Field.acidification <- ifelse(In.Field.acidification>= 0,In.Field.acidification, 0 )
  #   In.Field.acidification <- data.frame(In.Field.acidification)
  #
  # }
  #   In.Field.acidification <- ifelse((In.CaCO3[,1] * Ref.JRC.FA.lime) >= (In.F_SN[,1] * Ref.JRC.FA.N), In.CaCO3[,1] * Ref.JRC.FA.lime + In.F_ON[,1]*0.5* Ref.JRC.FA.N - In.F_SN[,1] * Ref.JRC.FA.N, In.F_ON[,1]*0.5* Ref.JRC.FA.N )
  #   In.Field.acidification <- data.frame(In.Field.acidification)
  #
  # }
  #


#### Calculation of CO2-emission from liming

  In.Field.acidification <- ifelse((In.CaCO3[,1] * 0.216) >= (In.F_SN[,1] * Ref.JRC.FA.N), In.CaCO3[,1] * 0.216 - In.F_SN[,1] * Ref.FA.N.EPA, 0 )
  In.Field.acidification <- data.frame(In.Field.acidification)  ### GREET1 Modell - Cai et al. 2014,  (US EPA 2014)
  names(In.Field.acidification) <- "In.Field.acidification"

  }

###### Weitere Emissionen (Blattdüngung) oder Offsetverschiebung #####
  # Weitere Emmisionen (Blattdüngung etc.) je ha konstant
  if(is.numeric(OtherEm_ha)) {
    GHG.ha.OtherEm_ha <- Out.Ertrag.Feucht * 0
    GHG.ha.OtherEm_ha[] <- Recycle(OtherEm_ha, GHG.ha.OtherEm_ha)
  }

#  if(!is.null(OtherEm_ha) & !is.numeric(OtherEm_ha)){
#    if(!is.null(Dataset)&OtherEm_ha %in% names (Dataset)){
#      GHG.ha.OtherEm_ha <- Dataset[OtherEm_ha]
#    }else{ print(paste("Fehler in 'OtherEm_ha':",
#                       "Erwartet wird Spaltenbezeichner für 'Dataset'",
#                       "oder ein numerischer Wert/Vektor"))
#    }
#  }
  names(GHG.ha.OtherEm_ha) <- "GHG.ha.OtherEm_ha"

##### Weitere Emmisionen je MJ konstant ####
  if(is.numeric(OtherEm_MJ)) {
    GHG.MJ.OtherEm_MJ <- Out.Ertrag.Feucht * 0
    GHG.MJ.OtherEm_MJ[] <- Recycle(OtherEm_MJ, GHG.MJ.OtherEm_MJ)
  }

#  if(!is.null(OtherEm_MJ) & !is.numeric(OtherEm_MJ)){
#    if(!is.null(Dataset)&OtherEm_MJ %in% names (Dataset)){
#      GHG.MJ.OtherEm_MJ <- Dataset[OtherEm_MJ]
#    }else{ print(paste("Fehler in 'OtherEm_MJ':",
#                       "Erwartet wird Spaltenbezeichner für 'Dataset'",
#                       "oder ein numerischer Wert/Vektor"))
#    }
#  }
  names(GHG.MJ.OtherEm_MJ) <- "GHG.MJ.OtherEm_MJ"


##### Emissionen für Verarbeitung und Transport ####
  # Allozierte Werte [g CO2eq/MJ RME]
  GHG.ep <- Out.Ertrag.Feucht * 0
  GHG.ep[] <- Recycle(ep, GHG.ep)
  names(GHG.ep) <- "GHG.ep"

  GHG.etd <-Out.Ertrag.Feucht * 0
  GHG.etd[] <- Recycle(etd, GHG.etd)
  names(GHG.etd) <- "GHG.etd"


#### Berechnung von System-Outputgrößen ####

  ##### Ertrag in MJ Raps/ha ####
  Out.Ertrag.RapsMJ_ha <- Out.Ertrag.DM * Ref.BG.Raps.MJ_kg
  names(Out.Ertrag.RapsMJ_ha) <- "Out.Ertrag.RapsMJ_ha"

##### oil yield in MJ oil/ha ####
  Out.Ertrag.OilMJ_ha <- Out.Ertrag.DM * Oil_9/0.91 * 37/100
  names(Out.Ertrag.OilMJ_ha) <- "Out.Ertrag.OilMJ_ha"

  ##### Ertrag in MJ RME/ha ####
#  Out.Ertrag.RMEMJ_ha <- Out.Ertrag.RapsMJ_ha * Ref.BG.conversion

## Neue Version mit Berechnung des RMEMJ_ha aus OilMJ_ha wg. Berücksichtigung Ölgehalt Raps.
## Faktor 0.95 ergibt sich aus BioGrace Id "Crude Vegetable Oil (MJ/ha)"/"Yield (in MJ biofuel / hectare cropland / year)"/
  Out.Ertrag.RMEMJ_ha <- Out.Ertrag.OilMJ_ha * 0.9538
  names(Out.Ertrag.RMEMJ_ha) <- "Out.Ertrag.RMEMJ_ha"

  ##### N-Abfuhr im Rapssamen [kg N/ha] ####
  Out.INRA.NHarvest <- Out.Ertrag.DM * Ref.INRA.Nc.Seed
  names(Out.INRA.NHarvest) <- "Out.INRA.NHarvest"
  Out.DueV.NHarvest <- Out.Ertrag.DM * Ref.DueV.Nc.Seed
  names(Out.DueV.NHarvest) <- "Out.DueV.NHarvest"

  ##### Biomasse (oberirdisch) ####
  Out.INRA.BiomassAG <- Out.Ertrag.DM * (1 / Ref.INRA.HI)
  names(Out.INRA.BiomassAG) <- "Out.INRA.BiomassAG"
  Out.DueV.BiomassAG <- Out.Ertrag.DM * (1 / Ref.DueV.HI)
  names(Out.DueV.BiomassAG) <- "Out.DueV.BiomassAGV"

  ##### N im Aufwuchs (oberidisch) ####
  Out.INRA.BiomassAGN <- Out.INRA.BiomassAG * Ref.INRA.Nc.AG
  names(Out.INRA.BiomassAGN) <- "Out.INRA.BiomassAGN"
  Out.DueV.BiomassAGN <- Out.DueV.BiomassAG * Ref.DueV.Nc.AG
  names(Out.DueV.BiomassAGN) <- "Out.DueV.BiomassAGN"

  ##### N in den Wurzeln ####
  Out.INRA.RootN <- Out.INRA.BiomassAG * Ref.INRA.Root_Shoot * Ref.INRA.Nc.Root
  names(Out.INRA.RootN) <- "Out.INRA.RootN"


  #### N in den Ernteresiduen für Tier 1 #####
  ##### Nach IPCC ####
  Int.IPCC.AG_DM <- Out.Ertrag.DM * Ref.IPCC.AG_DM.Slope + Ref.IPCC.AG_DM.Intercept
  names(Int.IPCC.AG_DM) <- "Int.IPCC.AG_DM"
  Int.IPCC.BGR_ha <- (Out.Ertrag.DM + Int.IPCC.AG_DM)*Ref.IPCC.R_S
  names(Int.IPCC.BGR_ha) <- "Int.IPCC.BGR_ha"
  In.F_CR.IPCC <- Int.IPCC.AG_DM * Ref.IPCC.N_AG + Int.IPCC.BGR_ha * Ref.IPCC.N_BG
  names(In.F_CR.IPCC) <- "In.F_CR.IPCC" # kg/ha/Jahr

  # Für Vergleiche:
  Out.IPCC.BiomassAG <- Int.IPCC.AG_DM + Out.Ertrag.DM
  names(Out.IPCC.BiomassAG) <- "Out.IPCC.BiomassAG"

  ##### Nach BioGrace ####
  Int.BG.AG_DM <- Out.Ertrag.DM * Ref.BG.AG_DM.Slope + Ref.BG.AG_DM.Intercept
  names(Int.BG.AG_DM) <- "Int.BG.AG_DM"
  Int.BG.BGR_ha <- (Out.Ertrag.DM + Int.BG.AG_DM)*Ref.BG.R_S
  names(Int.BG.BGR_ha) <- "Int.BG.BGR_ha"
  In.F_CR.BG <- Int.BG.AG_DM * Ref.BG.N_AG + Int.BG.BGR_ha * Ref.BG.N_BG
  names(In.F_CR.BG) <- "In.F_CR.BG" # kg/ha/Jahr

  # Für Vergleiche:
  Out.BG.BiomassAG <- Int.BG.AG_DM + Out.Ertrag.DM
  names(Out.BG.BiomassAG) <- "Out.BG.BiomassAG"

  ##### Mit Chalon-Data-Base ####
  In.F_CR.INRA <- Out.INRA.BiomassAGN + Out.INRA.RootN - Out.INRA.NHarvest
  names(In.F_CR.INRA) <- "In.F_CR.INRA"

  ##### Mit DüngeVO ####
  In.F_CR.DueV <- Out.DueV.BiomassAGN - Out.DueV.NHarvest
  names(In.F_CR.DueV) <- "In.F_CR.DueV"

  ##### Mit Sim-Werten ####
  str(Out.Ertrag.DM)
  In.F_CR.Sim <-  Out.Ertrag.DM*0 + SimResidue
  names(In.F_CR.Sim) <- "In.F_CR.Sim"


  #### Wahl der Berechnungsmethode für Crop Residues ####
  #print(str(SimResidue))

  if(ResidueCalc == "Chalon") {In.F_CR <- In.F_CR.INRA}
  if(ResidueCalc == "IPCC") {In.F_CR <- In.F_CR.IPCC}
  if(ResidueCalc == "DueV") {In.F_CR <- In.F_CR.DueV}
  if(ResidueCalc == "BioGrace") {In.F_CR <- In.F_CR.BG}
  if(ResidueCalc == "SimValue") {In.F_CR <- In.F_CR.Sim}
  if(!(ResidueCalc %in% list("Chalon", "IPCC", "DueV", "BioGrace", "SimValue"))){
    print(paste("Ungültiges Argument für 'ResidueCalc'.",
                "Mögliche Argumente: 'Chalon', 'IPCC', 'DueV', 'SimValue' "))
  }
  # In.F_CR <- as.numeric(unlist(In.F_CR))
  #print(In.F_CR)
  # print(attributes(In.F_CR))
  #SimN2ON

  #### Einlesen simulierter direkter N2O-Emissionen ####
  if(is.numeric(SimN2ON)) {
    In.SimN2ON <- Out.Ertrag.Feucht * 0
    SimN2ON[] <- Recycle(SimN2ON, In.SimN2ON)
  }

#  if(!is.null(SimN2ON) & !is.numeric(SimN2ON)){
#    if(!is.null(Dataset) & SimN2ON %in% names (Dataset)){
#      SimN2ON <- Dataset[SimN2ON]
#    }else{ print(paste("Fehler in 'SimN2ON':",
#                       "Erwartet wird Spaltenbezeichner für 'Dataset'",
#                       "oder ein numerischer Wert/Vektor"))
#    }
#  }
  names(SimN2ON) <- "SimN2ON"




  names(In.F_CR) <- "In.F_CR"
 # print(names(In.F_CR))
#  print(str(In.F_CR))



  ###                                 ###
####  Berechnung Klimagasemissionen  ####
  ###                                 ###

  ###  Direkte N2O-Emission im Feld   ###
  ### Berechnet nach IPCC 2006 Tier 1 ###

##### Detailaufschlüsselung (für Abb.)  ####
  # [kg N2O-N/ha]

  TotNInput <- In.F_SN+In.F_ON+In.F_CR
  TotNInput_F<- pmax(as.numeric(unlist(In.F_SN+In.F_ON)), 1)
  TotNInput_F <- as.data.frame(TotNInput_F)

##### Optionale Methoden für direkte N2O-Emissionen #####
  if (N2OSimMethod=="IPCC"){
    GHG.ha.N2O.Direct.N2ON.F_SN <- In.F_SN * Ref.IPCC.EF1.F_SN
    names(GHG.ha.N2O.Direct.N2ON.F_SN) <- "GHG.ha.N2O.Direct.N2ON.F_SN"
    GHG.ha.N2O.Direct.N2ON.F_ON <- In.F_ON * Ref.IPCC.EF1.F_ON
    names(GHG.ha.N2O.Direct.N2ON.F_ON) <- "GHG.ha.N2O.Direct.N2ON.F_ON"
    GHG.ha.N2O.Direct.N2ON.F_CR <- In.F_CR * Ref.IPCC.EF1.F_CR
    names(GHG.ha.N2O.Direct.N2ON.F_CR) <- "GHG.ha.N2O.Direct.N2ON.F_CR"
  }

  # in case external emissions are used they are allocated according to their share of total N input
  if(N2OSimMethod %in% c("HUME","Measurement", "EF_Germany", "DNDC", "MODE")) {
    GHG.ha.N2O.Direct.N2ON.F_SN <- SimN2ON*In.F_SN/TotNInput
    names(GHG.ha.N2O.Direct.N2ON.F_SN) <- "GHG.ha.N2O.Direct.N2ON.F_SN"
    GHG.ha.N2O.Direct.N2ON.F_ON <- SimN2ON*In.F_ON/TotNInput
    names(GHG.ha.N2O.Direct.N2ON.F_ON) <- "GHG.ha.N2O.Direct.N2ON.F_ON"
    GHG.ha.N2O.Direct.N2ON.F_CR <- SimN2ON*In.F_CR/TotNInput
    names(GHG.ha.N2O.Direct.N2ON.F_CR) <- "GHG.ha.N2O.Direct.N2ON.F_CR"
  }

  # for GNOC only the mineral N fertilizer is allocated
  if(N2OSimMethod== "GNOC"){
    GHG.ha.N2O.Direct.N2ON.F_SN <- SimN2ON*In.F_SN/TotNInput_F
    names(GHG.ha.N2O.Direct.N2ON.F_SN) <- "GHG.ha.N2O.Direct.N2ON.F_SN"
    GHG.ha.N2O.Direct.N2ON.F_ON <- SimN2ON*In.F_ON/TotNInput_F
    names(GHG.ha.N2O.Direct.N2ON.F_ON) <- "GHG.ha.N2O.Direct.N2ON.F_ON"
    GHG.ha.N2O.Direct.N2ON.F_CR <- In.F_CR * Ref.IPCC.EF1.F_CR
    names(GHG.ha.N2O.Direct.N2ON.F_CR) <- "GHG.ha.N2O.Direct.N2ON.F_CR"
  }


  ##### Umrechnung in kg N2O/ha ####
  GHG.ha.N2O.Direct.N2O.F_SN <- GHG.ha.N2O.Direct.N2ON.F_SN * Ref.IPCC.ef.N2ON
  names(GHG.ha.N2O.Direct.N2O.F_SN) <- "GHG.ha.N2O.Direct.N2O.F_SN"
  GHG.ha.N2O.Direct.N2O.F_ON <- GHG.ha.N2O.Direct.N2ON.F_ON * Ref.IPCC.ef.N2ON
  names(GHG.ha.N2O.Direct.N2O.F_ON) <- "GHG.ha.N2O.Direct.N2O.F_ON"
  GHG.ha.N2O.Direct.N2O.F_CR <- GHG.ha.N2O.Direct.N2ON.F_CR * Ref.IPCC.ef.N2ON
  names(GHG.ha.N2O.Direct.N2O.F_CR) <- "GHG.ha.N2O.Direct.N2O.F_CR"

  ##### Umrechnung in  kg CO2eq/ha ####
  GHG.ha.N2O.Direct.CO2eq.F_SN <- GHG.ha.N2O.Direct.N2O.F_SN * Ref.RED.ef.N2O
  names(GHG.ha.N2O.Direct.CO2eq.F_SN) <- "GHG.ha.N2O.Direct.CO2eq.F_SN"
  GHG.ha.N2O.Direct.CO2eq.F_ON <- GHG.ha.N2O.Direct.N2O.F_ON * Ref.RED.ef.N2O
  names(GHG.ha.N2O.Direct.CO2eq.F_ON) <- "GHG.ha.N2O.Direct.CO2eq.F_ON"
  GHG.ha.N2O.Direct.CO2eq.F_CR <- GHG.ha.N2O.Direct.N2O.F_CR * Ref.RED.ef.N2O
  names(GHG.ha.N2O.Direct.CO2eq.F_CR) <- "GHG.ha.N2O.Direct.CO2eq.F_CR"


  ###  Summe für Klimagasbilanz ####
  ### Auswahl welche berücksichtige werden sollen
  ### (Berechnungsweisen-Analyse)
  GHG.ha.N2O.Direct.N2ON <- Out.Ertrag.Feucht * 0 # kg N2O-N/ha
  GHG.ha.N2O.Direct.N2O <- Out.Ertrag.Feucht * 0 # kg N2O/ha
  GHG.ha.N2O.Direct.CO2eq <- Out.Ertrag.Feucht * 0 # kg C2Oeq/ha

  ##### Emissionen aus mineralischer N-Dgg ####
  # Ggf. mit Faktor
  if(IncludeSynthetic){
    GHG.ha.N2O.Direct.N2ON <- GHG.ha.N2O.Direct.N2ON + GHG.ha.N2O.Direct.N2ON.F_SN * as.numeric(IncludeSynthetic)
    GHG.ha.N2O.Direct.N2O <- GHG.ha.N2O.Direct.N2O + GHG.ha.N2O.Direct.N2O.F_SN * as.numeric(IncludeSynthetic)
    GHG.ha.N2O.Direct.CO2eq <- GHG.ha.N2O.Direct.CO2eq + GHG.ha.N2O.Direct.CO2eq.F_SN * as.numeric(IncludeSynthetic)
  }

  ##### Emissionen aus organischer N-Dgg ####
  # Ggf. mit Faktor
  if(IncludeOrganic){
    GHG.ha.N2O.Direct.N2ON <- GHG.ha.N2O.Direct.N2ON + GHG.ha.N2O.Direct.N2ON.F_ON * as.numeric(IncludeOrganic)
    GHG.ha.N2O.Direct.N2O <- GHG.ha.N2O.Direct.N2O + GHG.ha.N2O.Direct.N2O.F_ON * as.numeric(IncludeOrganic)
    GHG.ha.N2O.Direct.CO2eq <- GHG.ha.N2O.Direct.CO2eq + GHG.ha.N2O.Direct.CO2eq.F_ON * as.numeric(IncludeOrganic)
  }

  ##### Emissionen aus Ernteresiduen ####
  # Ggf. mit Faktor
  if(IncludeResidues){
    GHG.ha.N2O.Direct.N2ON <- GHG.ha.N2O.Direct.N2ON + GHG.ha.N2O.Direct.N2ON.F_CR * as.numeric(IncludeResidues)
    GHG.ha.N2O.Direct.N2O <- GHG.ha.N2O.Direct.N2O + GHG.ha.N2O.Direct.N2O.F_CR * as.numeric(IncludeResidues)
    GHG.ha.N2O.Direct.CO2eq <- GHG.ha.N2O.Direct.CO2eq + GHG.ha.N2O.Direct.CO2eq.F_CR * as.numeric(IncludeResidues)
  }

  names(GHG.ha.N2O.Direct.N2ON) <- "GHG.ha.N2O.Direct.N2ON"
  names(GHG.ha.N2O.Direct.N2O) <- "GHG.ha.N2O.Direct.N2O"
  names(GHG.ha.N2O.Direct.CO2eq) <- "GHG.ha.N2O.Direct.CO2eq"


  #### Indirekte N2O-Emission im Feld  ####
  ###        - Volatisation -         ###
  ### Berechnet nach IPCC 2006 Tier 1 ###

  ##### Detailaufschlüsselung für Abb.  ####
  # kg N2O-N/ha
  GHG.ha.N2O.Vola.N2ON.F_SN <- In.F_SN * Ref.IPCC.Frac_GASF * Ref.IPCC.EF4
  names(GHG.ha.N2O.Vola.N2ON.F_SN) <- "GHG.ha.N2O.Vola.N2ON.F_SN"
  GHG.ha.N2O.Vola.N2ON.F_ON <- In.F_ON * Ref.IPCC.Frac_GASM * Ref.IPCC.EF4
  names(GHG.ha.N2O.Vola.N2ON.F_ON) <- "GHG.ha.N2O.Vola.N2ON.F_ON"

  # kg N2O/ha
  GHG.ha.N2O.Vola.N2O.F_SN <- GHG.ha.N2O.Vola.N2ON.F_SN * Ref.IPCC.ef.N2ON
  names(GHG.ha.N2O.Vola.N2O.F_SN) <- "GHG.ha.N2O.Vola.N2O.F_SN"
  GHG.ha.N2O.Vola.N2O.F_ON <- GHG.ha.N2O.Vola.N2ON.F_ON * Ref.IPCC.ef.N2ON
  names(GHG.ha.N2O.Vola.N2O.F_ON) <- "GHG.ha.N2O.Vola.N2O.F_ON"

  # kg CO2eq/ha
  GHG.ha.N2O.Vola.CO2eq.F_SN <- GHG.ha.N2O.Vola.N2O.F_SN * Ref.RED.ef.N2O
  names(GHG.ha.N2O.Vola.CO2eq.F_SN) <- "GHG.ha.N2O.Vola.CO2eq.F_SN"
  GHG.ha.N2O.Vola.CO2eq.F_ON <- GHG.ha.N2O.Vola.N2O.F_ON * Ref.RED.ef.N2O
  names(GHG.ha.N2O.Vola.CO2eq.F_ON) <- "GHG.ha.N2O.Vola.CO2eq.F_ON"


  #### Summe für Klimagasbilanz ####
  ### Auswahl welche berücksichtige werden sollen
  ### (Berechnungsweisen-Analyse)
  GHG.ha.N2O.Vola.N2ON <- Out.Ertrag.Feucht * 0 # kg N2O-N/ha
  GHG.ha.N2O.Vola.N2O <- Out.Ertrag.Feucht * 0 # kg N2O/ha
  GHG.ha.N2O.Vola.CO2eq <- Out.Ertrag.Feucht * 0 # kg C2Oeq/ha

  ##### Emissionen aus mineralischer N-Dgg ####
  # Ggf. mit Faktor
  if(IncludeSynthetic){
    GHG.ha.N2O.Vola.N2ON <- GHG.ha.N2O.Vola.N2ON + GHG.ha.N2O.Vola.N2ON.F_SN * as.numeric(IncludeSynthetic)
    GHG.ha.N2O.Vola.N2O <- GHG.ha.N2O.Vola.N2O + GHG.ha.N2O.Vola.N2O.F_SN * as.numeric(IncludeSynthetic)
    GHG.ha.N2O.Vola.CO2eq <- GHG.ha.N2O.Vola.CO2eq + GHG.ha.N2O.Vola.CO2eq.F_SN * as.numeric(IncludeSynthetic)
  }

  ##### Emissionen aus organischer N-Dgg ####
  # Ggf. mit Faktor
  if(IncludeOrganic){
    GHG.ha.N2O.Vola.N2ON <- GHG.ha.N2O.Vola.N2ON + GHG.ha.N2O.Vola.N2ON.F_ON * as.numeric(IncludeOrganic)
    GHG.ha.N2O.Vola.N2O <- GHG.ha.N2O.Vola.N2O + GHG.ha.N2O.Vola.N2O.F_ON * as.numeric(IncludeOrganic)
    GHG.ha.N2O.Vola.CO2eq <- GHG.ha.N2O.Vola.CO2eq + GHG.ha.N2O.Vola.CO2eq.F_ON * as.numeric(IncludeOrganic)
  }

  names(GHG.ha.N2O.Vola.N2ON) <- "GHG.ha.N2O.Vola.N2ON"
  names(GHG.ha.N2O.Vola.N2O) <- "GHG.ha.N2O.Vola.N2O"
  names(GHG.ha.N2O.Vola.CO2eq) <- "GHG.ha.N2O.Vola.CO2eq"


  #### Indirekte N2O-Emission im Feld  ####
  ####       - Leaching/Runoff -       ####
  ### Berechnet nach IPCC 2006 Tier 1 ###

  ### Detailaufschlüsselung für Abb.  ###
  # kg N2O-N/ha

  if(sim==FALSE){
    GHG.ha.N2O.Leach.N2ON.F_SN <- In.F_SN * Ref.IPCC.Frac_LEACH * Ref.IPCC.EF5
    names(GHG.ha.N2O.Leach.N2ON.F_SN) <- "GHG.ha.N2O.Leach.N2ON.F_SN"
    GHG.ha.N2O.Leach.N2ON.F_ON <- In.F_ON * Ref.IPCC.Frac_LEACH * Ref.IPCC.EF5
    names(GHG.ha.N2O.Leach.N2ON.F_ON) <- "GHG.ha.N2O.Leach.N2ON.F_ON"
    GHG.ha.N2O.Leach.N2ON.F_CR <- In.F_CR * Ref.IPCC.Frac_LEACH * Ref.IPCC.EF5
    names(GHG.ha.N2O.Leach.N2ON.F_CR) <- "GHG.ha.N2O.Leach.N2ON.F_CR"
  }else{
    GHG.ha.N2O.Leach.N2ON.F_SN <- SimNLeach*In.F_SN/TotNInput* Ref.IPCC.EF5
    names(GHG.ha.N2O.Leach.N2ON.F_SN) <- "GHG.ha.N2O.Leach.N2ON.F_SN"
    GHG.ha.N2O.Leach.N2ON.F_ON <- SimNLeach*In.F_ON/TotNInput* Ref.IPCC.EF5
    names(GHG.ha.N2O.Leach.N2ON.F_ON) <- "GHG.ha.N2O.Leach.N2ON.F_ON"
    GHG.ha.N2O.Leach.N2ON.F_CR <- SimNLeach*In.F_CR/TotNInput* Ref.IPCC.EF5
    names(GHG.ha.N2O.Leach.N2ON.F_CR) <- "GHG.ha.N2O.Leach.N2ON.F_CR"
  }

  # kg N2O/ha
  GHG.ha.N2O.Leach.N2O.F_SN <- GHG.ha.N2O.Leach.N2ON.F_SN * Ref.IPCC.ef.N2ON
  names(GHG.ha.N2O.Leach.N2O.F_SN) <- "GHG.ha.N2O.Leach.N2O.F_SN"
  GHG.ha.N2O.Leach.N2O.F_ON <- GHG.ha.N2O.Leach.N2ON.F_ON * Ref.IPCC.ef.N2ON
  names(GHG.ha.N2O.Leach.N2O.F_ON) <- "GHG.ha.N2O.Leach.N2O.F_ON"
  GHG.ha.N2O.Leach.N2O.F_CR <- GHG.ha.N2O.Leach.N2ON.F_CR * Ref.IPCC.ef.N2ON
  names(GHG.ha.N2O.Leach.N2O.F_CR) <- "GHG.ha.N2O.Leach.N2O.F_CR"

  # kg CO2eq/ha
  GHG.ha.N2O.Leach.CO2eq.F_SN <- GHG.ha.N2O.Leach.N2O.F_SN * Ref.RED.ef.N2O
  names(GHG.ha.N2O.Leach.CO2eq.F_SN) <- "GHG.ha.N2O.Leach.CO2eq.F_SN"
  GHG.ha.N2O.Leach.CO2eq.F_ON <- GHG.ha.N2O.Leach.N2O.F_ON * Ref.RED.ef.N2O
  names(GHG.ha.N2O.Leach.CO2eq.F_ON) <- "GHG.ha.N2O.Leach.CO2eq.F_ON"
  GHG.ha.N2O.Leach.CO2eq.F_CR <- GHG.ha.N2O.Leach.N2O.F_CR * Ref.RED.ef.N2O
  names(GHG.ha.N2O.Leach.CO2eq.F_CR) <- "GHG.ha.N2O.Leach.CO2eq.F_CR"

  #### Summe für Klimagasbilanz ####
  ### Auswahl welche berücksichtige werden sollen
  ### (Berechnungsweisen-Analyse)
  GHG.ha.N2O.Leach.N2ON <- Out.Ertrag.Feucht * 0 # kg N2O-N/ha
  GHG.ha.N2O.Leach.N2O <- Out.Ertrag.Feucht * 0 # kg N2O/ha
  GHG.ha.N2O.Leach.CO2eq <- Out.Ertrag.Feucht * 0 # kg C2Oeq/ha

  ##### Emissionen aus mineralischer N-Dgg ####
  # Ggf. mit Faktor
  if(IncludeSynthetic){
    GHG.ha.N2O.Leach.N2ON <- GHG.ha.N2O.Leach.N2ON + GHG.ha.N2O.Leach.N2ON.F_SN * as.numeric(IncludeSynthetic)
    GHG.ha.N2O.Leach.N2O <- GHG.ha.N2O.Leach.N2O + GHG.ha.N2O.Leach.N2O.F_SN * as.numeric(IncludeSynthetic)
    GHG.ha.N2O.Leach.CO2eq <- GHG.ha.N2O.Leach.CO2eq + GHG.ha.N2O.Leach.CO2eq.F_SN * as.numeric(IncludeSynthetic)
  }

  ##### Emissionen aus organischer N-Dgg ####
  # Ggf. mit Faktor
  if(IncludeOrganic){
    GHG.ha.N2O.Leach.N2ON <- GHG.ha.N2O.Leach.N2ON + GHG.ha.N2O.Leach.N2ON.F_ON * as.numeric(IncludeOrganic)
    GHG.ha.N2O.Leach.N2O <- GHG.ha.N2O.Leach.N2O + GHG.ha.N2O.Leach.N2O.F_ON * as.numeric(IncludeOrganic)
    GHG.ha.N2O.Leach.CO2eq <- GHG.ha.N2O.Leach.CO2eq + GHG.ha.N2O.Leach.CO2eq.F_ON * as.numeric(IncludeOrganic)
  }

  ##### Emissionen aus Ernteresiduen ####
  # Ggf. mit Faktor
  if(IncludeResidues){
    GHG.ha.N2O.Leach.N2ON <- GHG.ha.N2O.Leach.N2ON + GHG.ha.N2O.Leach.N2ON.F_CR * as.numeric(IncludeResidues)
    GHG.ha.N2O.Leach.N2O <- GHG.ha.N2O.Leach.N2O + GHG.ha.N2O.Leach.N2O.F_CR * as.numeric(IncludeResidues)
    GHG.ha.N2O.Leach.CO2eq <- GHG.ha.N2O.Leach.CO2eq + GHG.ha.N2O.Leach.CO2eq.F_CR * as.numeric(IncludeResidues)
  }

  names(GHG.ha.N2O.Leach.N2ON) <- "GHG.ha.N2O.Leach.N2ON"
  names(GHG.ha.N2O.Leach.N2O) <- "GHG.ha.N2O.Leach.N2O"
  names(GHG.ha.N2O.Leach.CO2eq) <- "GHG.ha.N2O.Leach.CO2eq"


  #### Berechnung Emissionen bezogen Bezogen auf RME - Output [g CO2eq/MJ RME] ####
  GHG.MJ.N2O.Direct.N2ON.F_SN <- GHG.ha.N2O.Direct.N2ON.F_SN / Out.Ertrag.RMEMJ_ha * 1000
  names(GHG.MJ.N2O.Direct.N2ON.F_SN) <- "GHG.MJ.N2O.Direct.N2ON.F_SN"
  GHG.MJ.N2O.Direct.N2ON.F_ON <- GHG.ha.N2O.Direct.N2ON.F_ON / Out.Ertrag.RMEMJ_ha * 1000
  names(GHG.MJ.N2O.Direct.N2ON.F_ON) <- "GHG.MJ.N2O.Direct.N2ON.F_ON"
  GHG.MJ.N2O.Direct.N2ON.F_CR <- GHG.ha.N2O.Direct.N2ON.F_CR / Out.Ertrag.RMEMJ_ha * 1000
  names(GHG.MJ.N2O.Direct.N2ON.F_CR) <- "GHG.MJ.N2O.Direct.N2ON.F_CR"
  GHG.MJ.N2O.Direct.N2O.F_SN <- GHG.ha.N2O.Direct.N2O.F_SN / Out.Ertrag.RMEMJ_ha * 1000
  names(GHG.MJ.N2O.Direct.N2O.F_SN) <- "GHG.MJ.N2O.Direct.N2O.F_SN"
  GHG.MJ.N2O.Direct.N2O.F_ON <- GHG.ha.N2O.Direct.N2O.F_ON / Out.Ertrag.RMEMJ_ha * 1000
  names(GHG.MJ.N2O.Direct.N2O.F_ON) <- "GHG.MJ.N2O.Direct.N2O.F_ON"
  GHG.MJ.N2O.Direct.N2O.F_CR <- GHG.ha.N2O.Direct.N2O.F_CR / Out.Ertrag.RMEMJ_ha * 1000
  names(GHG.MJ.N2O.Direct.N2O.F_CR) <- "GHG.MJ.N2O.Direct.N2O.F_CR"
  GHG.MJ.N2O.Direct.CO2eq.F_SN <- GHG.ha.N2O.Direct.CO2eq.F_SN / Out.Ertrag.RMEMJ_ha * 1000
  names(GHG.MJ.N2O.Direct.CO2eq.F_SN) <- "GHG.MJ.N2O.Direct.CO2eq.F_SN"
  GHG.MJ.N2O.Direct.CO2eq.F_ON <- GHG.ha.N2O.Direct.CO2eq.F_ON / Out.Ertrag.RMEMJ_ha * 1000
  names(GHG.MJ.N2O.Direct.CO2eq.F_ON) <- "GHG.MJ.N2O.Direct.CO2eq.F_ON"
  GHG.MJ.N2O.Direct.CO2eq.F_CR <- GHG.ha.N2O.Direct.CO2eq.F_CR / Out.Ertrag.RMEMJ_ha * 1000
  names(GHG.MJ.N2O.Direct.CO2eq.F_CR) <- "GHG.MJ.N2O.Direct.CO2eq.F_CR"
  GHG.MJ.N2O.Direct.N2ON <- GHG.ha.N2O.Direct.N2ON / Out.Ertrag.RMEMJ_ha * 1000
  names(GHG.MJ.N2O.Direct.N2ON) <- "GHG.MJ.N2O.Direct.N2ON"
  GHG.MJ.N2O.Direct.N2O <- GHG.ha.N2O.Direct.N2O / Out.Ertrag.RMEMJ_ha * 1000
  names(GHG.MJ.N2O.Direct.N2O) <- "GHG.MJ.N2O.Direct.N2O"
  GHG.MJ.N2O.Direct.CO2eq <- GHG.ha.N2O.Direct.CO2eq / Out.Ertrag.RMEMJ_ha * 1000
  names(GHG.MJ.N2O.Direct.CO2eq) <- "GHG.MJ.N2O.Direct.CO2eq"
  GHG.MJ.N2O.Vola.N2ON.F_SN <- GHG.ha.N2O.Vola.N2ON.F_SN / Out.Ertrag.RMEMJ_ha * 1000
  names(GHG.MJ.N2O.Vola.N2ON.F_SN) <- "GHG.MJ.N2O.Vola.N2ON.F_SN"
  GHG.MJ.N2O.Vola.N2ON.F_ON <- GHG.ha.N2O.Vola.N2ON.F_ON / Out.Ertrag.RMEMJ_ha * 1000
  names(GHG.MJ.N2O.Vola.N2ON.F_ON) <- "GHG.MJ.N2O.Vola.N2ON.F_ON"
  GHG.MJ.N2O.Vola.N2O.F_SN <- GHG.ha.N2O.Vola.N2O.F_SN / Out.Ertrag.RMEMJ_ha * 1000
  names(GHG.MJ.N2O.Vola.N2O.F_SN) <- "GHG.MJ.N2O.Vola.N2O.F_SN"
  GHG.MJ.N2O.Vola.N2O.F_ON <- GHG.ha.N2O.Vola.N2O.F_ON / Out.Ertrag.RMEMJ_ha * 1000
  names(GHG.MJ.N2O.Vola.N2O.F_ON) <- "GHG.MJ.N2O.Vola.N2O.F_ON"
  GHG.MJ.N2O.Vola.CO2eq.F_SN <- GHG.ha.N2O.Vola.CO2eq.F_SN / Out.Ertrag.RMEMJ_ha * 1000
  names(GHG.MJ.N2O.Vola.CO2eq.F_SN) <- "GHG.MJ.N2O.Vola.CO2eq.F_SN"
  GHG.MJ.N2O.Vola.CO2eq.F_ON <- GHG.ha.N2O.Vola.CO2eq.F_ON / Out.Ertrag.RMEMJ_ha * 1000
  names(GHG.MJ.N2O.Vola.CO2eq.F_ON) <- "GHG.MJ.N2O.Vola.CO2eq.F_ON"
  GHG.MJ.N2O.Vola.N2ON <- GHG.ha.N2O.Vola.N2ON / Out.Ertrag.RMEMJ_ha * 1000
  names(GHG.MJ.N2O.Vola.N2ON) <- "GHG.MJ.N2O.Vola.N2ON"
  GHG.MJ.N2O.Vola.N2O <- GHG.ha.N2O.Vola.N2O / Out.Ertrag.RMEMJ_ha * 1000
  names(GHG.MJ.N2O.Vola.N2O) <- "GHG.MJ.N2O.Vola.N2O"
  GHG.MJ.N2O.Vola.CO2eq <- GHG.ha.N2O.Vola.CO2eq / Out.Ertrag.RMEMJ_ha * 1000
  names(GHG.MJ.N2O.Vola.CO2eq) <- "GHG.MJ.N2O.Vola.CO2eq"
  GHG.MJ.N2O.Leach.N2ON.F_SN <- GHG.ha.N2O.Leach.N2ON.F_SN / Out.Ertrag.RMEMJ_ha * 1000
  names(GHG.MJ.N2O.Leach.N2ON.F_SN) <- "GHG.MJ.N2O.Leach.N2ON.F_SN"
  GHG.MJ.N2O.Leach.N2ON.F_ON <- GHG.ha.N2O.Leach.N2ON.F_ON / Out.Ertrag.RMEMJ_ha * 1000
  names(GHG.MJ.N2O.Leach.N2ON.F_ON) <- "GHG.MJ.N2O.Leach.N2ON.F_ON"
  GHG.MJ.N2O.Leach.N2ON.F_CR <- GHG.ha.N2O.Leach.N2ON.F_CR / Out.Ertrag.RMEMJ_ha * 1000
  names(GHG.MJ.N2O.Leach.N2ON.F_CR) <- "GHG.MJ.N2O.Leach.N2ON.F_CR"
  GHG.MJ.N2O.Leach.N2O.F_SN <- GHG.ha.N2O.Leach.N2O.F_SN / Out.Ertrag.RMEMJ_ha * 1000
  names(GHG.MJ.N2O.Leach.N2O.F_SN) <- "GHG.MJ.N2O.Leach.N2O.F_SN"
  GHG.MJ.N2O.Leach.N2O.F_ON <- GHG.ha.N2O.Leach.N2O.F_ON / Out.Ertrag.RMEMJ_ha * 1000
  names(GHG.MJ.N2O.Leach.N2O.F_ON) <- "GHG.MJ.N2O.Leach.N2O.F_ON"
  GHG.MJ.N2O.Leach.N2O.F_CR <- GHG.ha.N2O.Leach.N2O.F_CR / Out.Ertrag.RMEMJ_ha * 1000
  names(GHG.MJ.N2O.Leach.N2O.F_CR) <- "GHG.MJ.N2O.Leach.N2O.F_CR"
  GHG.MJ.N2O.Leach.CO2eq.F_SN <- GHG.ha.N2O.Leach.CO2eq.F_SN / Out.Ertrag.RMEMJ_ha * 1000
  names(GHG.MJ.N2O.Leach.CO2eq.F_SN) <- "GHG.MJ.N2O.Leach.CO2eq.F_SN"
  GHG.MJ.N2O.Leach.CO2eq.F_ON <- GHG.ha.N2O.Leach.CO2eq.F_ON / Out.Ertrag.RMEMJ_ha * 1000
  names(GHG.MJ.N2O.Leach.CO2eq.F_ON) <- "GHG.MJ.N2O.Leach.CO2eq.F_ON"
  GHG.MJ.N2O.Leach.CO2eq.F_CR <- GHG.ha.N2O.Leach.CO2eq.F_CR / Out.Ertrag.RMEMJ_ha * 1000
  names(GHG.MJ.N2O.Leach.CO2eq.F_CR) <- "GHG.MJ.N2O.Leach.CO2eq.F_CR"
  GHG.MJ.N2O.Leach.N2ON <- GHG.ha.N2O.Leach.N2ON / Out.Ertrag.RMEMJ_ha * 1000
  names(GHG.MJ.N2O.Leach.N2ON) <- "GHG.MJ.N2O.Leach.N2ON"
  GHG.MJ.N2O.Leach.N2O <- GHG.ha.N2O.Leach.N2O / Out.Ertrag.RMEMJ_ha * 1000
  names(GHG.MJ.N2O.Leach.N2O) <- "GHG.MJ.N2O.Leach.N2O"
  GHG.MJ.N2O.Leach.CO2eq <- GHG.ha.N2O.Leach.CO2eq / Out.Ertrag.RMEMJ_ha * 1000
  names(GHG.MJ.N2O.Leach.CO2eq) <- "GHG.MJ.N2O.Leach.CO2eq"

  ##### Berecnung Emissionssummen ####
  GHG.ha.N2O.Summe.N2ON <- GHG.ha.N2O.Direct.N2ON
  if(IncludeVolatil){
    GHG.ha.N2O.Summe.N2ON <- GHG.ha.N2O.Summe.N2ON + GHG.ha.N2O.Vola.N2ON}
  if(IncludeVolatil){
    GHG.ha.N2O.Summe.N2ON <- GHG.ha.N2O.Summe.N2ON + GHG.ha.N2O.Leach.N2ON}
  names(GHG.ha.N2O.Summe.N2ON) <- "GHG.ha.N2O.Summe.N2ON"
  GHG.ha.N2O.Summe.N2O <- GHG.ha.N2O.Direct.N2O
  if(IncludeVolatil){
    GHG.ha.N2O.Summe.N2O <- GHG.ha.N2O.Summe.N2O + GHG.ha.N2O.Vola.N2O}
  if(IncludeVolatil){
    GHG.ha.N2O.Summe.N2O <- GHG.ha.N2O.Summe.N2O + GHG.ha.N2O.Leach.N2O}
  names(GHG.ha.N2O.Summe.N2O) <- "GHG.ha.N2O.Summe.N2O"
  GHG.ha.N2O.Summe.CO2eq <- GHG.ha.N2O.Direct.CO2eq
  if(IncludeVolatil){
    GHG.ha.N2O.Summe.CO2eq <- GHG.ha.N2O.Summe.CO2eq + GHG.ha.N2O.Vola.CO2eq}
  if(IncludeVolatil){
    GHG.ha.N2O.Summe.CO2eq <- GHG.ha.N2O.Summe.CO2eq + GHG.ha.N2O.Leach.CO2eq}
  names(GHG.ha.N2O.Summe.CO2eq) <- "GHG.ha.N2O.Summe.CO2eq"
  GHG.MJ.N2O.Summe.N2ON <- GHG.ha.N2O.Summe.N2ON / Out.Ertrag.RMEMJ_ha * 1000
  names(GHG.MJ.N2O.Summe.N2ON) <- "GHG.MJ.N2O.Summe.N2ON"
  GHG.MJ.N2O.Summe.N2O <- GHG.ha.N2O.Summe.N2O / Out.Ertrag.RMEMJ_ha * 1000
  names(GHG.MJ.N2O.Summe.N2O) <- "GHG.MJ.N2O.Summe.N2O"
  GHG.MJ.N2O.Summe.CO2eq <- GHG.ha.N2O.Summe.CO2eq / Out.Ertrag.RMEMJ_ha * 1000
  names(GHG.MJ.N2O.Summe.CO2eq) <- "GHG.MJ.N2O.Summe.CO2eq"


  #### Klimagasemissionen Bereitstellung ####
  #####      Dünger & Betriebsmittel      ####
  # kg CO2eq/ha

  ###### Düngung ####
  GHG.ha.NDuenger <- In.F_SN * Ref.BG.ef.N
  names(GHG.ha.NDuenger) <- "GHG.ha.NDuenger"
  GHG.ha.N2O <- GHG.ha.N2O.Summe.CO2eq
  names(GHG.ha.N2O) <- "GHG.ha.N2O"
  GHG.ha.CaCO3Duenger <- In.CaCO3 * Ref.BG.ef.CaCO3
  names(GHG.ha.CaCO3Duenger) <- "GHG.ha.CaCO3Duenger"
  GHG.ha.K2ODuenger <- In.K2O * Ref.BG.ef.K2O
  names(GHG.ha.K2ODuenger) <- "GHG.ha.K2ODuenger"
  GHG.ha.P2O5Duenger <- In.P2O5 * Ref.BG.ef.P2O5
  names(GHG.ha.P2O5Duenger) <- "GHG.ha.P2O5Duenger"

  ###### Betriebsmittel #####
  GHG.ha.PSM <- In.PSM * Ref.BG.ef.PSM
  names(GHG.ha.PSM) <- "GHG.ha.PSM"
  GHG.ha.Saatgut <- In.Saatgut * Ref.BG.ef.Saatgut
  names(GHG.ha.Saatgut) <- "GHG.ha.Saatgut"
  GHG.ha.Field.acidification <- In.Field.acidification                                                                       # ist schon in kg/ha Co2
  names (GHG.ha.Field.acidification) <- "GHG.ha.Field.acidification"
  GHG.ha.Diesel <- In.Diesel * (Ref.BG.ef.Diesel.l + Ref.BG.ef.Diesel.use.CH4.N2O)                                             # neu in BGII
  names(GHG.ha.Diesel) <- "GHG.ha.Diesel"

  GHG.ha.OtherEm_MJ <- GHG.MJ.OtherEm_MJ * Out.Ertrag.RMEMJ_ha / 1000
  names(GHG.ha.OtherEm_MJ) <- "GHG.ha.OtherEm_MJ"

  GHG.ha.Drying <- In.Dry.Therm_MJ * Ref.BG.ef.NaturalGas + In.Dry.Electric * Ref.BG.ef.Electricity
  names(GHG.ha.Drying) <- "GHG.ha.Drying"
  GHG.ha.Summe <- (GHG.ha.NDuenger + GHG.ha.N2O + GHG.ha.CaCO3Duenger +
                     GHG.ha.K2ODuenger + GHG.ha.P2O5Duenger + GHG.ha.PSM +
                     GHG.ha.Saatgut + GHG.ha.Diesel+ GHG.ha.Field.acidification + GHG.ha.OtherEm_ha + GHG.ha.OtherEm_MJ)

  if(IncludeDrying){
    GHG.ha.Summe <- GHG.ha.Summe + GHG.ha.Drying
  }
  names(GHG.ha.Summe) <- "GHG.ha.Summe"

  ##### Berechnung Emissionen Düngung/Betriebsmittel bezogen auf MJ RME #####
  # g CO2eq/MJ RME
  GHG.MJ.NDuenger <- GHG.ha.NDuenger / Out.Ertrag.RMEMJ_ha * 1000
  names(GHG.MJ.NDuenger) <- "GHG.MJ.NDuenger"
  GHG.MJ.N2O <- GHG.ha.N2O / Out.Ertrag.RMEMJ_ha * 1000
  names(GHG.MJ.N2O) <- "GHG.MJ.N2O"
  GHG.MJ.CaCO3Duenger <- GHG.ha.CaCO3Duenger / Out.Ertrag.RMEMJ_ha * 1000
  names(GHG.MJ.CaCO3Duenger) <- "GHG.MJ.CaCO3Duenger"
  GHG.MJ.K2ODuenger <- GHG.ha.K2ODuenger / Out.Ertrag.RMEMJ_ha * 1000
  names(GHG.MJ.K2ODuenger) <- "GHG.MJ.K2ODuenger"
  GHG.MJ.P2O5Duenger <- GHG.ha.P2O5Duenger / Out.Ertrag.RMEMJ_ha * 1000
  names(GHG.MJ.P2O5Duenger) <- "GHG.MJ.P2O5Duenger"
  GHG.MJ.PSM <- GHG.ha.PSM / Out.Ertrag.RMEMJ_ha * 1000
  names(GHG.MJ.PSM) <- "GHG.MJ.PSM"
  GHG.MJ.Saatgut <- GHG.ha.Saatgut / Out.Ertrag.RMEMJ_ha * 1000
  names(GHG.MJ.Saatgut) <- "GHG.MJ.Saatgut"
  GHG.MJ.Field.acidification <- GHG.ha.Field.acidification / Out.Ertrag.RMEMJ_ha * 1000
  names (GHG.MJ.Field.acidification) <- "GHG.MJ.Field.acidification"
  GHG.MJ.Diesel <- GHG.ha.Diesel / Out.Ertrag.RMEMJ_ha * 1000
  names(GHG.MJ.Diesel) <- "GHG.MJ.Diesel"
  GHG.MJ.OtherEm_ha <- GHG.ha.OtherEm_ha / Out.Ertrag.RMEMJ_ha * 1000
  names(GHG.MJ.OtherEm_ha) <- "GHG.MJ.OtherEm_ha"
  GHG.MJ.Drying <- GHG.ha.Drying / Out.Ertrag.RMEMJ_ha * 1000
  names(GHG.MJ.Drying) <- "GHG.MJ.Drying"
  GHG.MJ.Summe <- (GHG.MJ.NDuenger + GHG.MJ.N2O + GHG.MJ.CaCO3Duenger +
                     GHG.MJ.K2ODuenger + GHG.MJ.P2O5Duenger + GHG.MJ.PSM +
                     GHG.MJ.Saatgut + GHG.MJ.Field.acidification + GHG.MJ.Diesel + GHG.MJ.OtherEm_ha + GHG.MJ.OtherEm_MJ)
  if(IncludeDrying){
    GHG.MJ.Summe <- GHG.MJ.Summe + GHG.MJ.Drying
  }
  names(GHG.MJ.Summe) <- "GHG.MJ.Summe"

  #### Emissionen Anbau NICHT Alloziert ####
  GHG.eec_nonallo <- GHG.MJ.Summe
  names(GHG.eec_nonallo) <- "GHG.eec_nonallo"

  #### Emissionen Anbau Alloziert ####
  GHG.eec <- GHG.eec_nonallo * Ref.BG.allocation
  names(GHG.eec) <- "GHG.eec"

  #### Emissionen Biokraftstoff Alloziert ####
  GHG.EB <- GHG.eec + GHG.ep + GHG.etd
  names(GHG.EB) <- "GHG.EB"

  #### Einsparpotential ####
  GHG.Savings <- (Ref.RED.ef.Diesel - GHG.EB) / Ref.RED.ef.Diesel *100
  names(GHG.Savings) <- "GHG.Savings"


  ####  Output generieren ####

  for (i in 1:length(Show)) {
    if(!(Show[[i]] %in% ValidArg.Show)){
      print(paste("WARNUNG:", Show[[i]],
                  "ist kein gültiges Argument für die Ausgabe"))
    }
  }
  # In.F_CR <- as.numeric(unlist(In.F_CR))

  # print(names(In.F_SN))
  # print(names(In.F_CR))
  # print(str(In.F_SN))
  # print(str(In.F_CR))

  OutputFrame <- list(Out.Ertrag.Feucht, Out.Feuchte, Out.Ertrag.9prz,
                      Out.Ertrag.DM, Out.Ertrag.RapsMJ_ha, Out.Ertrag.OilMJ_ha, Out.Ertrag.RMEMJ_ha,
                      In.F_SN, In.F_ON, In.F_CR, In.CaCO3, In.K2O, In.P2O5, In.PSM,
                      In.Saatgut,In.Field.acidification, In.Diesel, In.Dry.Therm_MJ, In.Dry.Electric,
                      GHG.MJ.OtherEm_ha, GHG.MJ.OtherEm_MJ, GHG.ha.OtherEm_ha, GHG.ha.OtherEm_MJ,
                      GHG.eec_nonallo, GHG.eec, GHG.ep, GHG.etd, GHG.EB, GHG.Savings)

  if(("complete" %in% Show) | ("eec" %in% Show) | ("eec.ha" %in% Show)){
    OutputFrame <- list(as.data.frame(OutputFrame),
                        GHG.ha.NDuenger, GHG.ha.CaCO3Duenger, GHG.ha.K2ODuenger,
                        GHG.ha.P2O5Duenger, GHG.ha.PSM, GHG.ha.Saatgut, GHG.ha.N2O, GHG.ha.Field.acidification,
                        GHG.ha.Diesel, GHG.ha.Drying, GHG.ha.Summe)
  }

  if(("complete" %in% Show) | ("eec" %in% Show) | ("eec.MJ" %in% Show)){
    OutputFrame <- list(as.data.frame(OutputFrame),
                        GHG.MJ.NDuenger, GHG.MJ.CaCO3Duenger, GHG.MJ.K2ODuenger,
                        GHG.MJ.P2O5Duenger, GHG.MJ.PSM, GHG.MJ.Saatgut, GHG.MJ.N2O, GHG.MJ.Field.acidification,
                        GHG.MJ.Diesel, GHG.MJ.Drying, GHG.MJ.Summe)
  }

  if(("complete" %in% Show) | ("N2O" %in% Show) | ("N2O.detail.ha" %in% Show) |
     ("N2O.detail.MJ" %in% Show) | ("N2O.detail.ha.more" %in% Show) | ("N2O.detail.MJ.more" %in% Show)){
    OutputFrame <- list(as.data.frame(OutputFrame),
                        GHG.ha.N2O.Summe.N2ON, GHG.ha.N2O.Summe.N2O, GHG.ha.N2O.Summe.CO2eq,
                        GHG.MJ.N2O.Summe.N2ON, GHG.MJ.N2O.Summe.N2O, GHG.MJ.N2O.Summe.CO2eq)
  }

  if(("complete" %in% Show) | ("N2O.detail.ha" %in% Show) | ("N2O.detail.ha.more" %in% Show)){
    OutputFrame <- list(as.data.frame(OutputFrame),
                        GHG.ha.N2O.Direct.N2ON, GHG.ha.N2O.Direct.N2O, GHG.ha.N2O.Direct.CO2eq,
                        GHG.ha.N2O.Vola.N2ON, GHG.ha.N2O.Vola.N2O, GHG.ha.N2O.Vola.CO2eq,
                        GHG.ha.N2O.Leach.N2ON, GHG.ha.N2O.Leach.N2O, GHG.ha.N2O.Leach.CO2eq)
  }

  if(("complete" %in% Show) | ("N2O.detail.ha.more" %in% Show)){
    OutputFrame <- list(as.data.frame(OutputFrame),
                        GHG.ha.N2O.Direct.N2ON.F_SN, GHG.ha.N2O.Direct.N2ON.F_ON,
                        GHG.ha.N2O.Direct.N2ON.F_CR, GHG.ha.N2O.Direct.N2O.F_SN,
                        GHG.ha.N2O.Direct.N2O.F_ON, GHG.ha.N2O.Direct.N2O.F_CR,
                        GHG.ha.N2O.Direct.CO2eq.F_SN, GHG.ha.N2O.Direct.CO2eq.F_ON,
                        GHG.ha.N2O.Direct.CO2eq.F_CR, GHG.ha.N2O.Vola.N2ON.F_SN,
                        GHG.ha.N2O.Vola.N2ON.F_ON, GHG.ha.N2O.Vola.N2O.F_SN,
                        GHG.ha.N2O.Vola.N2O.F_ON, GHG.ha.N2O.Vola.CO2eq.F_SN,
                        GHG.ha.N2O.Vola.CO2eq.F_ON, GHG.ha.N2O.Leach.N2ON.F_SN,
                        GHG.ha.N2O.Leach.N2ON.F_ON, GHG.ha.N2O.Leach.N2ON.F_CR,
                        GHG.ha.N2O.Leach.N2O.F_SN, GHG.ha.N2O.Leach.N2O.F_ON,
                        GHG.ha.N2O.Leach.N2O.F_CR, GHG.ha.N2O.Leach.CO2eq.F_SN,
                        GHG.ha.N2O.Leach.CO2eq.F_ON, GHG.ha.N2O.Leach.CO2eq.F_CR)
  }

  if(("complete" %in% Show) | ("N2O.detail.MJ" %in% Show) | ("N2O.detail.MJ.more" %in% Show)){
    OutputFrame <- list(as.data.frame(OutputFrame),
                        GHG.MJ.N2O.Direct.N2ON, GHG.MJ.N2O.Direct.N2O,
                        GHG.MJ.N2O.Direct.CO2eq, GHG.MJ.N2O.Vola.N2ON,
                        GHG.MJ.N2O.Vola.N2O, GHG.MJ.N2O.Vola.CO2eq,
                        GHG.MJ.N2O.Leach.N2ON, GHG.MJ.N2O.Leach.N2O,
                        GHG.MJ.N2O.Leach.CO2eq)
  }

  if(("complete" %in% Show) | ("N2O.detail.MJ.more" %in% Show)){
    OutputFrame <- list(as.data.frame(OutputFrame),
                        GHG.MJ.N2O.Direct.N2ON.F_SN, GHG.MJ.N2O.Direct.N2ON.F_ON,
                        GHG.MJ.N2O.Direct.N2ON.F_CR, GHG.MJ.N2O.Direct.N2O.F_SN,
                        GHG.MJ.N2O.Direct.N2O.F_ON, GHG.MJ.N2O.Direct.N2O.F_CR,
                        GHG.MJ.N2O.Direct.CO2eq.F_SN, GHG.MJ.N2O.Direct.CO2eq.F_ON,
                        GHG.MJ.N2O.Direct.CO2eq.F_CR, GHG.MJ.N2O.Vola.N2ON.F_SN,
                        GHG.MJ.N2O.Vola.N2ON.F_ON, GHG.MJ.N2O.Vola.N2O.F_SN,
                        GHG.MJ.N2O.Vola.N2O.F_ON, GHG.MJ.N2O.Vola.CO2eq.F_SN,
                        GHG.MJ.N2O.Vola.CO2eq.F_ON, GHG.MJ.N2O.Leach.N2ON.F_SN,
                        GHG.MJ.N2O.Leach.N2ON.F_ON, GHG.MJ.N2O.Leach.N2ON.F_CR,
                        GHG.MJ.N2O.Leach.N2O.F_SN, GHG.MJ.N2O.Leach.N2O.F_ON,
                        GHG.MJ.N2O.Leach.N2O.F_CR, GHG.MJ.N2O.Leach.CO2eq.F_SN,
                        GHG.MJ.N2O.Leach.CO2eq.F_ON, GHG.MJ.N2O.Leach.CO2eq.F_CR)
  }

  if("compare" %in% Show){
    OutputFrame <- list(as.data.frame(OutputFrame),
                        In.F_CR.DueV, In.F_CR.INRA, In.F_CR.IPCC, Out.DueV.BiomassAG,
                        Out.DueV.BiomassAGN, Out.DueV.NHarvest, Out.INRA.BiomassAG,
                        Out.INRA.BiomassAGN, Out.INRA.NHarvest, Out.INRA.RootN,
                        Out.IPCC.BiomassAG)
  }


  # as.data.frame(OutputFrame)
  if (!is.null(Treatment)){
  OutputFrame <- list(Treatment, as.data.frame(OutputFrame))}else{
    OutputFrame <- as.data.frame(OutputFrame)
  }
  DF <- as.data.frame(OutputFrame)
  return(DF)
}






