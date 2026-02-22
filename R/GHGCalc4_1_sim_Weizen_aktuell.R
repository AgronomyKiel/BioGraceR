
# GHG-Rechner für Bioethanol aus Weizen ##################

# Version des Abschlussberichts FNR überarbeitet für Bachelorarbeit Kohlmetz
# Emissionen des Einsatz von Feldberegnung
# Ertrag in kg/ha angeben!!!
# Standardmethode für Ernteresiduen ist ResidueCalc="BioGrace"!!!
# IncludeDrying nun standardmaessig = FALSE!!!
# eec nach BG I Version 4d, April  2015

# rm(list = ls(all=T))
# library(tidyverse)

#' Title GHGCalculator_Wheat_4_1
#'
#' @param Ertrag
#' seed yield in (kg/ha) give as fresh dry matter
#' @param Treatment
#' Optional Vector with Treatment/Variant-Codes
#' @param pH
#' @param Feuchte
#' water content of OSR seed given as (\%), default value is 9\%
#' @param SimNLeach
#' simulated nitrogen leaching in (kg/ha)
#' @param SimN2ON
#' simulated N2O-N emissions in (kg/ha)
#' @param SimResidue
#' #'  Optional direct quantification of N-amount in Crop residues in (kg N/ha), by using own estimates or from simulation modelling
#' @param sim
#' logical (vector) for using own estimates/simulated input values
#' @param Nmineral
#' mineral N-fertilisation in (kg N/ha), default value is 114 kg N/ha
#' @param Norgan
#' organic N fertilization in (kg/ha)
#' @param P2O5
#' Optional fertilisation rate of phosphorous in (kg P2O5/ha), if not given, fertilisation is assumed to replace P offtake by seeds
#' @param K2O
#' Optional fertilisation rate of potassium in (kg K2O/ha), if not given fertilisation is assumed to replace K offtake by seeds
#' @param CaCO3
#' Optional rate of liming in (kg CaCO3/ha)
#' @param Field.acidification
#' @param Dataset
#' @param PSM
#' Input of plant protection products in (kg/ha)
#' @param Saatgut
#' seed input (kg/ha)
#' @param Diesel
#' Diesel use for field operations default value is 83.3 l/ha
#' @param Irrigation_mm
#' amount of irrigation in (mm), default value is 0 mm
#' @param Irri.Energy
#' energy use for irrigation in (MJ/mm), default value is 0 MJ/mm
#' @param Irri.ef.Energy
#' energy use for irrigation in (gCO2eq/MJ used energy), default value is 0 gCO2eq/MJ
#' @param ep
#' emissions from production of bioethanol (g CO2eq/MJ), default is 15.1 g CO2eq/MJ,
#' Red II(2018)other cereals excluding maize ethanol (natural gas  as  process fuel in conventional boiler)
#' @param etd
#' emissions from transport and distribution of bioethanol (g CO2eq/MJ), default is 2.2 g CO2eq/MJ,
#' @param ResidueCalc
#'  Optional method for calculating the amount of N in crop residues, default value is "BioGrace"
#' @param N2OCalcMethod
#' "Tier1", "custom" Tier1: EF1=0.01, custom = Verwendung spezifischer Dis.EF1 wie unten gegeben
#' @param N2OSimMethod
#' "HUME" ,"GNOC", "DNDC" und "MODE" angeben, wenn SimN2ON nicht 0, steuert die Aufteilung direkter Emissionen auf F_SN, F_ON und F_ER
#' @param IncludeOrganic
#' @param IncludeResidues
#' @param IncludeSynthetic
#' @param IncludeDrying
#' @param IncludeVolatil
#' @param IncludeLeaching
#' @param OtherEm_ha
#' @param OtherEm_MJ
#' @param Dis.EF1.F_SN
#' @param Dis.EF1.F_ON
#' @param Dis.EF1.F_CR
#' @param Dis.IPCC.EF4
#' @param Dis.CAS.Frac_GASF
#' @param Frac_NLeach
#' @param EF.SN.Prod
#' @param Show
#'
#' @return data frame with outputs according to the Show parameter
#' @export
#'
#' @examples
GHGCalculator_Wheat_4_1 <- function(Ertrag, Treatment, pH=NA, Feuchte=13.5,
                                SimNLeach=0, SimN2ON=0, SimResidue=0, sim=FALSE,  ## sim=TRUE , wenn N-Auswaschung SimNLeach als WErt vorgegeben
                                Nmineral=114,Norgan=0,
                                P2O5=NULL, K2O=NULL, CaCO3=NULL,  ## Wenn keine Werte gegeben, dann werden aus Ertrag jeweilige Düngehöhen errechnet
                                Field.acidification=NULL,
                                Dataset=NULL, ## Wenn Daten als Dataset übergeben werden, entsprechen die entsprechenden Funktionsargumente dann den Namen der Spalten
                                PSM=5.853 , Saatgut=36, Diesel=91.3, # Aufwandmengen in kg, kg und Liter je ha
                                Irrigation_mm=0, Irri.Energy=0 , Irri.ef.Energy=0,  # Beregnung Irrigation_mm [mm], Irri.Energy [MJ/mm], Irri.ef.Energy [gCO2eq/MJ used energy]
                                ep=15.1, # Red II(2018)other cereals excluding maize ethanol (natural gas  as  process fuel in conventional boiler) ,
                                etd=2.2,  #Red II(2018)
                                ResidueCalc="BioGrace",
                                N2OCalcMethod="Tier1",# "Tier1", "custom" Tier1: EF1=0.01, custom = Verwendung spezifischer Dis.EF1 wie unten gegeben
                                N2OSimMethod= "IPCC", # "HUME" ,"GNOC", "DNDC" und "MODE" angeben, wenn SimN2ON nicht 0, steuert die Aufteilung direkter Emissionen auf F_SN, F_ON und F_ER
                                IncludeOrganic=TRUE,  # "Include..." includes processes and N sources for calculating direct an indirect N2O-Emissions for Output
                                IncludeResidues=TRUE,
                                IncludeSynthetic=TRUE,
                                IncludeDrying=FALSE,
                                IncludeVolatil=TRUE,
                                IncludeLeaching=TRUE,  ## "Include..." includes processes and N sources for calculating direct an indirect N2O-Emissions for Output
                                OtherEm_ha=0, # Additional Emissions in kg CO2eq/ha
                                OtherEm_MJ=0, # Additional Emissions in kg CO2eq/MJ
                                Dis.EF1.F_SN = 0.016, #Synthetic fertilizer inputs in wet climates IPCC(2019)
                                Dis.EF1.F_ON = 0.006, #Other N inputs in wet climates IPCC (2019)
                                Dis.EF1.F_CR = 0.006, #Other N inputs in wet climates IPCC (2019)
                                Dis.IPCC.EF4 = 0.014, #wet climates IPCC (2019)
                                Dis.CAS.Frac_GASF = 0.05, #Fraction of N input prone to volatilization (IPCC, 2019)
                                Frac_NLeach = 0.24, # fraction of N input prone to leaching (IPCC, 2019)
								                EF.SN.Prod = 3.469, # N fertilizer production emissions # recalculated JRC-Mix 3.951 kg CO2eq / kg N
                                Show=list("complete", "eec", "eec.ha", "eec.MJ",          # definition of output variables, see ValidArg.Show
                                          "N2O", "N2O.detail.ha", "N2O.detail.MJ",
                                          "N2O.detail.ha.more", "N2O.detail.MJ.more",
                                          "compare", "internal")){


  # Hilfsfunktion zum Auslesen der Uebergebenen
  # Parameter. Recycelt einen numerischen Vektor
  # bis Laenge eines Zielvektors oder einer Datenfeld-
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

  ValidArg.Show <- list("complete", "eec", "eec.ha",                          ### A
                        "eec.MJ", "N2O", "N2O.detail.ha", "N2O.detail.MJ",
                        "N2O.detail.ha.more", "N2O.detail.MJ.more",
                        "compare", "internal", "minimal")

  ValidArg.N2OCalc <- list("Tier1", "custom")
  stopifnot(N2OCalcMethod %in% ValidArg.N2OCalc)

  #
  ValidArg.N2OSim <- list("IPCC", "IPCC_2019","GNOC", "HUME", "Measurement", "EF_Germany", "DNDC", "MODE")
  stopifnot(N2OSimMethod %in% ValidArg.N2OSim)

  #
  # Konstanten ##############################
  #

  Durchschnitt.Ertrag.DE <- 7517 ##DM
  # German Federal Statistical Office-Destatis. German wheat production area and annual wheat yield. Available
  # online: https://www.destatis.de/DE/ZahlenFakten/Wirtschaftsbereiche/LandForstwirtschaftFischerei/
  # FeldfruechteGruenland/Tabellen/FeldfruechteZeitreihe.html (accessed on 15.11.2021)

  ### Referenz-/ Stantardwerte ###
  Ref.RED.ef.gasoline <-  94         ##red II  ##93.3 g CO2eq/MJ (Edward, 2019)
  Ref.RED.ef.CO2 <- 1
  Ref.RED.ef.N2O <- 273             ##IPCC Sixth Assessment Report (2021)
  Ref.RED.ef.CH4 <- 29.2

  # Wenn nicht anders angegeben
  # alle Werte in kg CO2eq/kg
  Ref.BG.ef.N <- EF.SN.Prod
  Ref.BG.ef.P2O5 <- 0.5417   #(Edward et al., 2019) (S.11)
  Ref.BG.ef.K2O <- 0.416700   #(Edward et al., 2019)
  Ref.BG.ef.CaCO3 <- 0.03906964 #(Edward et al., 2019)
  Ref.BG.ef.PSM <- 12.0106546
  Ref.BG.ef.Saatgut <- 0.2839
  Ref.BG.ef.Electricity <- 0.17031314                                      #BG II additional Standardvalues --> IFEU calculations [kg CO2eq/MJ]
  Ref.BG.ef.NaturalGas <- 0.066                                            #JRC Report EUR 28349 (2019)
  Ref.BG.ef.Diesel.MJ <- 95.1 / 1000
  Ref.BG.Diesel.MJ_kg <- 43.1
  Ref.BG.Diesel.kg_l <- 0.832
  Ref.BG.Diesel.MJ_l <- Ref.BG.Diesel.kg_l * Ref.BG.Diesel.MJ_kg
  Ref.BG.ef.Diesel.l <- Ref.BG.ef.Diesel.MJ * Ref.BG.Diesel.MJ_l
  Ref.BG.ef.Diesel.use.CH4.N2O <- 0.00098615313225058
  Ref.BG.Weizen.MJ_kg <- 17                                               # reiner Energiegehalt, keine Konversion; neu nach BG II
  # Ref.JRC.FA.lime <- 0.079                                                #JRC Report EUR 28349 (2019)
  # Ref.JRC.FA.N <- 0.798                                                   #JRC Report EUR 28349 (2019)
  Ref.FA.N.EPA <- 0.386                                 #  emission from acidification effect from N fertilizer ((0.216 g CO2/g CaCO3) berechnet nach Cai et al.,2014 ## EPA(Modell GREET1_2014)
  Ref.BG.conversion <- 0.5313
  Ref.BG.allocation <- 0.551
  Kalkung.kg_MJWeizen <- 0.00257 #(Edward et al. 2019, S.73)

  # Werte fuer direkte und indirekte N2O-Emission  -  muessten nach Biograce II ueber GNOC berechnet werden
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
  Ref.IPCC.AG_DM.Slope <- 1.61  #GNOC 2014
  Ref.IPCC.AG_DM.Intercept <- 0.41 #GNOC 2014
  Ref.IPCC.R_S <- 0.23 #GNOC 2014
  Ref.IPCC.N_AG <- 0.006 #GNOC 2014
  Ref.IPCC.N_BG <- 0.009 #GNOC 2014

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

  Ref.IPCC.Frac_LEACH <- Frac_NLeach  # (IPCC, 2019)
  Ref.IPCC.EF5 <- 0.011


  # Ernteresiduen nach BioGrace

  Ref.BG.AG_DM.Slope <- 1.51
  Ref.BG.AG_DM.Intercept <- 0.52
  Ref.BG.N_AG <- 0.006
  Ref.BG.N_BG <- 0.009
  Ref.BG.R_S <- 0.24



  Ref.DueV.Nc.Seed <- (1.81 / 100) / 0.86 # Bezug in der DüV ist 14% (12% Protein)!!!
  Ref.DueV.HI <- 0.86 / (0.86 + 0.8 * 0.86) # In DueV ist Korn:Stroh gegeben das auf Basis 13.5% bzw 14%
  Ref.DueV.Nc.AG <- ((2.21 /100 ) / 0.86) / (1 / Ref.DueV.HI)
  # Verordnung ?ber die Anwendung von D?ngemitteln,
  # Bodenhilfsstoffen, Kultursubstraten und Pflanzenhilfsmitteln
  # nach den Grunds?tzen der guten fachlichen Praxis beim D?ngen2
  # (D?ngeverordnung - D?V) S.34




  Ref.JRC.dry.Therm_MJ_kgH2O <- 4.32 #MJ/kg H2O (Edward et al.2019)
  Ref.JRC.dry.Ele.kWh_t <-1.2 #kWh/t Gut(feucht)
  Ref.JRC.dry.Ele.MJ_t <- Ref.JRC.dry.Ele.kWh_t * 3.6 #MJ/t Gut(feucht)
  Ref.Duev.Entz.P <- 3.5203 # (Element; Basis 13.5% Feuchte! kg/t, Duev 2020 )
  Ref.LWK.Entz.K <- 5 # (Element; Basis 13.5% Feuchte! kg/t, Duev 2020 )
  Const.m.P <- 30.974
  Const.m.K <- 39.098
  Const.m.O <- 15.999
  Const.m.P2O5 <- 2 * Const.m.P + 5 * Const.m.O
  Const.m.K2O <- 2 * Const.m.K + Const.m.O
  Const.CV.PtoP2O5 <- Const.m.P2O5 / (2 * Const.m.P)
  Const.CV.KtoK2O <- Const.m.K2O / (2 * Const.m.K)


  # Zuweisung der übergebenen Eingangsparameter
  # Für spätere Ausgabe müssen alle Parameter
  # als Vektoren der gleichen Länge wie der
  # Ertragsvektor vorliegen.

  # Ertrag <- as.numeric(unlist(Ertrag))
  # Nmineral <- as.numeric(unlist(Nmineral))
  # Treatment <- as.character(unlist(Treatment))
  # SimResidue <- as.numeric(unlist(SimResidue))
  # Norgan <- as.numeric(unlist(Norgan))
  # SimNLeach <- as.numeric(unlist(SimNLeach))
  # SimN2ON <- as.numeric(unlist(SimN2ON))


  #  Dataset als Data Frame (Falls liste o.ä.)
  if(!is.null(Dataset)){Dataset <- as.data.frame(Dataset)}

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

  if(!is.null(Ertrag)&!is.numeric(Ertrag)){
    if(!is.null(Dataset) & "Ertrag" %in% names (Dataset)){
      Out.Ertrag.Feucht <- Dataset$Ertrag
    }else{ print(paste("Fehler in 'Ertrag':",
                       "Erwartet wird Spaltenbezeichner fuer 'Dataset'",
                       "oder ein numerischer Wert/Vektor"))
    }
  }
  names(Out.Ertrag.Feucht) <- "Out.Ertrag.Feucht"


  # Feuchte
  if(is.numeric(Feuchte)) {
    Out.Feuchte <- Out.Ertrag.Feucht * 0
    Out.Feuchte[] <- Recycle(Feuchte / 100, Out.Feuchte)
  }

  if(!is.null(Feuchte) & !is.numeric(Feuchte)){
    if(!is.null(Dataset) & "Feuchte" %in% names (Dataset)){
      Out.Feuchte <- Dataset$Feuchte /100
    }else{ print(paste("Fehler in 'Feuchte':",
                       "Erwartet wird Spaltenbezeichner fuer 'Dataset'",
                       "oder ein numerischer Wert/Vektor"))
    }
  }
  names(Out.Feuchte) <- "Out.Feuchte"


  ## Ertrag als DM
  Out.Ertrag.DM <- Out.Ertrag.Feucht * (1 - Out.Feuchte)

  names(Out.Ertrag.DM) <- "Out.Ertrag.DM"

  # Ertrag standardisiert auf 13.5% Feuchte

  Out.Ertrag.13.5prz <- Out.Ertrag.DM / 0.865
  names(Out.Ertrag.13.5prz) <- "Out.Ertrag.13.5prz"


  ## Treatment name
  if(!is.null(Dataset)){
    if(is.null(Treatment) & "Treatment" %in% names (Dataset)) {
      Treatment<- Dataset$Treatment

    } else {

      print(paste("Fehler in 'Treatment':",
                  "Erwartet wird Spaltenbezeichner feur 'Dataset'"))

    }


  } else {

    if(!is.null(Treatment)){
      Treatment <- Treatment
      Treatment<- as.data.frame(Treatment)
    } else {
      Treatment <- "NA"
      Treatment <- as.data.frame(Treatment)
    }
  }

  ### Berechnung Trocknung ###
  # Notwendige zu verdampfende Wassermenge bis 13.5% H2O
  Int.Dry.H2O <- Out.Ertrag.Feucht * 0
  for (i in 1:nrow(Out.Feuchte)){
    if(Out.Feuchte[i,] > 0.135){
      Int.Dry.H2O[i,] <- Out.Ertrag.Feucht[i,] * (Out.Feuchte[i,] - (1 - Out.Feuchte[i,]) / 0.865 * 0.135)
    }else{ Int.Dry.H2O[i,] <- 0
    }
  }
  names(Int.Dry.H2O) <- "Int.Dry.H2O"

  # Wärmebedarf Trocknung MJ/ha
  if(IncludeDrying){
    In.Dry.Therm_MJ <- Int.Dry.H2O * Ref.JRC.dry.Therm_MJ_kgH2O
  }else{In.Dry.Therm_MJ <- Int.Dry.H2O * 0}
  names(In.Dry.Therm_MJ) <- "In.Dry.Therm_MJ"

  # Bedarf elektr. Energie MJ/ha
  if(IncludeDrying){
    In.Dry.Electric <- Out.Ertrag.Feucht / 1000 * Ref.JRC.dry.Ele.MJ_t
  }else{In.Dry.Electric <- Out.Ertrag.Feucht * 0}
  names(In.Dry.Electric) <- "In.Dry.Electric"


  ### Duengung ###
  # Mineralische Düngung
  if(is.numeric(Nmineral)) {
    In.F_SN <- Out.Ertrag.Feucht * 0
    In.F_SN[] <- Recycle(Nmineral, In.F_SN)
  }

  if(!is.null(Nmineral) & !is.numeric(Nmineral)){
    if(!is.null(Dataset) & "Nmineral" %in% names (Dataset)){
      In.F_SN <- Dataset$Nmineral
    }else{ print(paste("Fehler in 'Nmineral':",
                       "Erwartet wird Spaltenbezeichner fuer 'Dataset'",
                       "oder ein numerischer Wert/Vektor"))
    }
  }
  names(In.F_SN) <- "In.F_SN"

  # Organische Düngung
  if(is.numeric(Norgan)) {
    In.F_ON <- Out.Ertrag.Feucht * 0
    In.F_ON[] <- Recycle(Norgan, In.F_ON)
  }

  if(!is.null(Norgan) & !is.numeric(Norgan)){
    if(!is.null(Dataset) & "Norgan" %in% names (Dataset)){
      In.F_ON <- Dataset$Norgan
    }else{ print(paste("Fehler in 'Norgan':",
                       "Erwartet wird Spaltenbezeichner fuer 'Dataset'",
                       "oder ein numerischer Wert/Vektor"))
    }
  }
  names(In.F_ON) <- "In.F_ON"

  ##pH
  if(is.numeric(pH)) {
    pH_in <- Out.Ertrag.Feucht * 0
    pH[] <- Recycle(pH, pH_in)
  } else {
    #names(Dataset)
    if(!is.null(pH) & !is.numeric(pH)){
      if(!is.null(Dataset) & "pH" %in% names (Dataset)){
        pH <- Dataset$pH
      }else{ print(paste("Fehler in 'pH':",
                         "Erwartet wird Spaltenbezeichner für 'Dataset'",
                         "oder ein numerischer Wert/Vektor"))
      }
    }
  }
  names(pH) <- "pH"



  # P2O5
  if(is.null(P2O5)) {
    In.P2O5 <- Out.Ertrag.13.5prz / 1000 * Ref.Duev.Entz.P * Const.CV.PtoP2O5
  }
  #In.P2O5 <- 7517/.87/1000 * Ref.Duev.Entz.P * Const.CV.PtoP2O5
  if(is.numeric(P2O5)) {
    In.P2O5 <- Out.Ertrag.Feucht * 0 # Düngung muss als Vektor
    In.P2O5[] <- Recycle(P2O5, In.P2O5) # der selben Länge wie Datensatz vorliegen
  }

  if(!is.null(P2O5) & !is.numeric(P2O5)){
    if(!is.null(Dataset) & "P2O5" %in% names (Dataset)){
      In.P2O5 <- Dataset$P2O5
    }else{ print(paste("Fehler in 'P2O5':",
                       "Erwartet wird Spaltenbezeichner fuer 'Dataset'",
                       "oder ein numerischer Wert/Vektor"))
    }
  }
  names(In.P2O5) <- "In.P2O5"

  # K2O
  if(is.null(K2O)) {
    In.K2O <- Out.Ertrag.13.5prz / 1000 * Ref.LWK.Entz.K * Const.CV.KtoK2O
  }
  # In.K2O <- 7517/.87/ 1000 * Ref.LWK.Entz.K * Const.CV.KtoK2O
  if(is.numeric(K2O)) {
    In.K2O <- Out.Ertrag.Feucht * 0 # Duengung muss als Vektor
    In.K2O[] <- Recycle(K2O, In.K2O) # der selben Länge wie Datensatz vorliegen
  }
  if(!is.null(K2O)&!is.numeric(K2O)){
    if(!is.null(Dataset)& "K2O" %in% names (Dataset)){
      In.K2O <- Dataset$K2O
    }else{ print(paste("Fehler in 'K2O':",
                       "Erwartet wird Spaltenbezeichner fuer 'Dataset'",
                       "oder ein numerischer Wert/Vektor"))
    }
  }
  names(In.K2O) <- "In.K2O"



  # CaCO3

  if(is.null(CaCO3)) {
    In.CaCO3 <- Durchschnitt.Ertrag.DE * (1-Out.Feuchte) * Ref.BG.Weizen.MJ_kg * Kalkung.kg_MJWeizen
  }
  if(is.numeric(CaCO3)) {
    In.CaCO3 <- Out.Ertrag.Feucht * 0
    In.CaCO3[] <- Recycle(CaCO3, In.CaCO3)
  }
  if(!is.null(CaCO3) & !is.numeric(CaCO3)){
    if(!is.null(Dataset) & "CaCO3" %in% names (Dataset)){
      In.CaCO3 <- Dataset$CaCO3
    }else{ print(paste("Fehler in 'CaCO3':",
                       "Erwartet wird Spaltenbezeichner fuer 'Dataset'",
                       "oder ein numerischer Wert/Vektor"))
    }
  }
  names(In.CaCO3) <- "In.CaCO3"


  # PSM
  if(is.numeric(PSM)) {
    In.PSM <- Out.Ertrag.Feucht * 0
    In.PSM[] <- Recycle(PSM, In.PSM)
  }

  if(!is.null(PSM) & !is.numeric(PSM)){
    if(!is.null(Dataset) & "PSM" %in% names (Dataset)){
      In.PSM <- Dataset$PSM
    }else{ print(paste("Fehler in 'PSM':",
                       "Erwartet wird Spaltenbezeichner fuer 'Dataset'",
                       "oder ein numerischer Wert/Vektor"))
    }
  }
  names(In.PSM) <- "In.PSM"

  # Saatgut
  if(is.numeric(Saatgut)) {
    In.Saatgut <- Out.Ertrag.Feucht * 0
    In.Saatgut[] <- Recycle(Saatgut, In.Saatgut)
  }

  if(!is.null(Saatgut) & !is.numeric(Saatgut)){
    if(!is.null(Dataset) & "Saatgut" %in% names (Dataset)){
      In.Saatgut <- Dataset$Saatgut
    }else{ print(paste("Fehler in 'Saatgut':",
                       "Erwartet wird Spaltenbezeichner fuer 'Dataset'",
                       "oder ein numerischer Wert/Vektor"))
    }
  }
  names(In.Saatgut) <- "In.Saatgut"

  # Diesel
  if(is.numeric(Diesel)) {
    In.Diesel <- Out.Ertrag.Feucht * 0
    In.Diesel[] <- Recycle(Diesel, In.Diesel)
  }
  if(!is.null(Diesel) & !is.numeric(Diesel)){
    if(!is.null(Dataset) & "Diesel" %in% names (Dataset)){
      In.Diesel <- Dataset$Diesel
    }else{ print(paste("Fehler in 'Diesel':",
                       "Erwartet wird Spaltenbezeichner fuer 'Dataset'",
                       "oder ein numerischer Wert/Vektor"))
    }
  }
  names(In.Diesel) <- "In.Diesel"

  # Bewässerungsmenge  In.Irri.Amount_mm *In.Irri.EnergyUseMJ_mm* In.ef.Irri.Energy
  if(is.numeric(Irrigation_mm)) {
    In.Irri.Amount_mm <- Out.Ertrag.Feucht * 0
    In.Irri.Amount_mm[] <- Recycle(Irrigation_mm, In.Irri.Amount_mm)
  }
  if(!is.null(Irrigation_mm) & !is.numeric(Irrigation_mm)){
    if(!is.null(Dataset) & "Irrigation_mm" %in% names (Dataset)){
      In.Irri.Amount_mm <- Dataset$Irrigation_mm
    }else{ print(paste("Fehler in 'Irrigation_mm':",
                       "Erwartet wird Spaltenbezeichner fuer 'Dataset'",
                       "oder ein numerischer Wert/Vektor"))
    }
  }
  names(In.Irri.Amount_mm) <- "In.Irri.Amount_mm"

  # Energiebedarf Bewässerung
  if(is.numeric(Irri.Energy)) {
    In.Irri.EnergyUseMJ_mm <- Out.Ertrag.Feucht * 0
    In.Irri.EnergyUseMJ_mm[] <- Recycle(Irri.Energy, In.Irri.EnergyUseMJ_mm)
  }
  if(!is.null(Irri.Energy) & !is.numeric(Irri.Energy)){
    if(!is.null(Dataset) & "Irri.Energy" %in% names (Dataset)){
      In.Irri.EnergyUseMJ_mm <- Dataset$Irri.Energy
    }else{ print(paste("Fehler in 'Irri.Energy':",
                       "Erwartet wird Spaltenbezeichner fuer 'Dataset'",
                       "oder ein numerischer Wert/Vektor"))
    }
  }
  names(In.Irri.EnergyUseMJ_mm) <- "In.Irri.EnergyUseMJ_mm"

  # Emissionsfaktor der genutzten Energieform für Bewässerung
  if(is.numeric(Irri.ef.Energy)) {
    In.ef.Irri.Energy <- Out.Ertrag.Feucht * 0
    In.ef.Irri.Energy[] <- Recycle(Irri.ef.Energy, In.ef.Irri.Energy)
  }
  if(!is.null(Irri.ef.Energy) & !is.numeric(Irri.ef.Energy)){
    if(!is.null(Dataset) & "Irri.ef.Energy" %in% names (Dataset)){
      In.ef.Irri.Energy <- Dataset$Irri.ef.Energy
    }else{ print(paste("Fehler in 'Irri.ef.Energy':",
                       "Erwartet wird Spaltenbezeichner fuer 'Dataset'",
                       "oder ein numerischer Wert/Vektor"))
    }
  }
  names(In.ef.Irri.Energy) <- "In.ef.Irri.Energy"

  # Extern berechnetet direkte N2ON - Emissionen (SimN2ON)

  if(is.numeric(SimN2ON)) {
    In.SimN2ON <- Out.Ertrag.Feucht * 0
    In.SimN2ON[] <- Recycle(SimN2ON, In.SimN2ON)
  }
  if(!is.null(SimN2ON) & !is.numeric(SimN2ON)){
    if(!is.null(Dataset) & "SimN2ON" %in% names (Dataset)){
      In.SimN2ON <- Dataset$SimN2ON
    }else{ print(paste("Fehler in 'SimN2ON':",
                       "Erwartet wird Spaltenbezeichner für 'Dataset'",
                       "oder ein numerischer Wert/Vektor"))
    }
  }
  names(In.SimN2ON) <- "In.SimN2ON"

  # Emissionen - Field.acidification

  if(is.numeric(Field.acidification)) {
    In.Field.acidification <- Out.Ertrag.Feucht * 0
    In.Field.acidification[] <- Recycle(Field.acidification, In.Field.acidification)
  }
  if(!is.null(Field.acidification) & !is.numeric(Field.acidification)){
    if(!is.null(Dataset) & "Field.acidification" %in% names (Dataset)){
      In.Field.acidification <- Dataset$Field.acidification
    }else{ print(paste("Fehler in 'Field.acidification':",
                       "Erwartet wird Spaltenbezeichner fuer 'Dataset'",
                       "oder ein numerischer Wert/Vektor"))
    }
  }
  # if(is.null(Field.acidification)) {
  #  In.Field.acidification <- In.CaCO3 * Ref.JRC.FA.lime + In.F_ON*0.5* Ref.JRC.FA.N - In.F_SN * Ref.JRC.FA.N        #in kg CO2/ha
  # }
  if(is.null(Field.acidification)) {

    #   In.Field.acidification <- ifelse (pH[,1] >=6.4, ### JRC depending on pH
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

    In.Field.acidification <- ifelse((In.CaCO3[,1] * 0.216) >= (In.F_SN[,1] * Ref.FA.N.EPA), In.CaCO3[,1] * 0.216 - In.F_SN[,1] * Ref.FA.N.EPA, 0 )
    In.Field.acidification <- data.frame(In.Field.acidification)  ### GREET1 Modell - Cai et al. 2014,  (US EPA 2014)

    names(In.Field.acidification) <- "In.Field.acidification"
  }

  ### Weitere Emissionen (Blattdüngung) oder Offsetverschiebung ###
  ### Weitere Emissionen (Blattdüngung etc.) je ha konstant

  if(is.numeric(OtherEm_ha)) {
    GHG.ha.OtherEm_ha <- Out.Ertrag.Feucht * 0
    GHG.ha.OtherEm_ha[] <- Recycle(OtherEm_ha, GHG.ha.OtherEm_ha)
  }

  if(!is.null(OtherEm_ha) & !is.numeric(OtherEm_ha)){
    if(!is.null(Dataset)& "OtherEm_ha" %in% names (Dataset)){
      GHG.ha.OtherEm_ha <- Dataset$OtherEm_ha
    }else{ GHG.ha.OtherEm_ha=0
    }
  }

  names(GHG.ha.OtherEm_ha) <- "GHG.ha.OtherEm_ha"

  # Weitere Emissionen je MJ konstant
  if(is.numeric(OtherEm_MJ)) {
    GHG.MJ.OtherEm_MJ <- Out.Ertrag.Feucht * 0
    GHG.MJ.OtherEm_MJ[] <- Recycle(OtherEm_MJ, GHG.MJ.OtherEm_MJ)
  }

  if(!is.null(OtherEm_MJ) & !is.numeric(OtherEm_MJ)){
    if(!is.null(Dataset)& "OtherEm_MJ" %in% names (Dataset)){
      GHG.MJ.OtherEm_MJ <- Dataset$OtherEm_MJ
    }else{ print(paste("Fehler in 'OtherEm_MJ':",
                       "Erwartet wird Spaltenbezeichner fuer 'Dataset'",
                       "oder ein numerischer Wert/Vektor"))
    }
  }
  names(GHG.MJ.OtherEm_MJ) <- "GHG.MJ.OtherEm_MJ"


  ### Emissionen für Verarbeitung und Transport ###
  # Allozierte Werte [g CO2eq/MJ Ethanol]
  GHG.ep <- Out.Ertrag.Feucht * 0
  GHG.ep[] <- Recycle(ep, GHG.ep)
  names(GHG.ep) <- "GHG.ep"

  GHG.etd <-Out.Ertrag.Feucht * 0
  GHG.etd[] <- Recycle(etd, GHG.etd)
  names(GHG.etd) <- "GHG.etd"


  ### Berechnung von System-Outputgrößen #############################
  # Ertrag in MJ Weizen/ha
  Out.Ertrag.WeizenMJ_ha <- Out.Ertrag.DM * Ref.BG.Weizen.MJ_kg
  names(Out.Ertrag.WeizenMJ_ha) <- "Out.Ertrag.WeizenMJ_ha"



  #Ertrag in MJ Ethanol/ha
  Out.Ertrag.EthanolMJ_ha <- Out.Ertrag.WeizenMJ_ha * Ref.BG.conversion
  names(Out.Ertrag.EthanolMJ_ha) <- "Out.Ertrag.EthanolMJ_ha"


  # N-Abfuhr im seeds [kg N/ha]
  Out.DueV.NHarvest <- Out.Ertrag.DM * Ref.DueV.Nc.Seed
  names(Out.DueV.NHarvest) <- "Out.DueV.NHarvest"

  # Biomasse (oberirdisch)
  #
  Out.DueV.BiomassAG <- Out.Ertrag.DM * (1 / Ref.DueV.HI)
  names(Out.DueV.BiomassAG) <- "Out.DueV.BiomassAGV"

  # N im Aufwuchs (oberidisch)

  Out.DueV.BiomassAGN <- Out.DueV.BiomassAG * Ref.DueV.Nc.AG
  names(Out.DueV.BiomassAGN) <- "Out.DueV.BiomassAGN"


  ### N in den Ernteresiduen fuer Tier 1 ###
  # Nach IPCC
  Int.IPCC.AG_DM <- Out.Ertrag.DM * Ref.IPCC.AG_DM.Slope + Ref.IPCC.AG_DM.Intercept
  names(Int.IPCC.AG_DM) <- "Int.IPCC.AG_DM"
  Int.IPCC.BGR_ha <- (Out.Ertrag.DM + Int.IPCC.AG_DM)*Ref.IPCC.R_S
  names(Int.IPCC.BGR_ha) <- "Int.IPCC.BGR_ha"
  In.F_CR.IPCC <- Int.IPCC.AG_DM * Ref.IPCC.N_AG + Int.IPCC.BGR_ha * Ref.IPCC.N_BG
  names(In.F_CR.IPCC) <- "In.F_CR.IPCC" # kg/ha/Jahr

  # Fuer Vergleiche:
  Out.IPCC.BiomassAG <- Int.IPCC.AG_DM + Out.Ertrag.DM
  names(Out.IPCC.BiomassAG) <- "Out.IPCC.BiomassAG"

  # Nach BioGrace
  Int.BG.AG_DM <- Out.Ertrag.DM * Ref.BG.AG_DM.Slope + Ref.BG.AG_DM.Intercept
  names(Int.BG.AG_DM) <- "Int.BG.AG_DM"
  Int.BG.BGR_ha <- (Out.Ertrag.DM + Int.BG.AG_DM)*Ref.BG.R_S
  names(Int.BG.BGR_ha) <- "Int.BG.BGR_ha"
  In.F_CR.BG <- Int.BG.AG_DM * Ref.BG.N_AG + Int.BG.BGR_ha * Ref.BG.N_BG
  names(In.F_CR.BG) <- "In.F_CR.BG" # kg/ha/Jahr

  # Fuer Vergleiche:
  Out.BG.BiomassAG <- Int.BG.AG_DM + Out.Ertrag.DM
  names(Out.BG.BiomassAG) <- "Out.BG.BiomassAG"


  # Mit DüngeVO
  In.F_CR.DueV <- Out.DueV.BiomassAGN - Out.DueV.NHarvest
  names(In.F_CR.DueV) <- "In.F_CR.DueV"

  # Mit Sim-Werten

  str(Out.Ertrag.DM)

  if(is.numeric(SimResidue)) {
    In.F_CR.Sim <-  Out.Ertrag.DM*0 + SimResidue
  }
  names(In.F_CR.Sim) <- "In.F_CR.Sim"
  if(!is.null(SimResidue) & !is.numeric(SimResidue)){
    if(!is.null(Dataset) & "SimResidue" %in% names (Dataset)){
      In.F_CR.Sim <- Dataset$SimResidue
    }else{ print(paste("Fehler in 'SimResidue':",
                       "Erwartet wird Spaltenbezeichner für 'Dataset'",
                       "oder ein numerischer Wert/Vektor"))
    }
  }
  names(In.F_CR.Sim) <- "In.F_CR.Sim"


  ### Wahl der Berechnungsmethode für CR ###

  if(ResidueCalc == "IPCC") {In.F_CR <- In.F_CR.IPCC}
  if(ResidueCalc == "DueV") {In.F_CR <- In.F_CR.DueV}
  if(ResidueCalc == "BioGrace") {In.F_CR <- In.F_CR.BG}
  if(ResidueCalc == "SimValue") {In.F_CR <- In.F_CR.Sim}

  if(!(ResidueCalc %in% list("IPCC", "DueV", "BioGrace", "SimValue"))){
    print(paste("Ungueltiges Argument fuer 'ResidueCalc'.",
                "Moegliche Argumente: 'IPCC', 'DueV', 'SimValue' "))
  }

  # In.F_CR <- as.numeric(unlist(In.F_CR))
  names(In.F_CR) <- "In.F_CR"
  # print(names(In.F_CR))
  # print(str(In.F_CR))


  ###  Berechnung Klimagasemissionen  #######


  ###  Direkte N2O-Emission im Feld   ###
  ### Berechnet nach IPCC 2006 Tier 1 ###

  ### Detailaufschlüsselung für Abb.  kg N2O-N/ha

  TotNInput <- In.F_SN+In.F_ON+In.F_CR
  TotNInput_F<- pmax(as.numeric(unlist(In.F_SN+In.F_ON)), 1) ## Variable für die Aufteilung der Emissionen auf N-Quellen
  TotNInput_F <- as.data.frame(TotNInput_F)

  if (N2OSimMethod%in% c("IPCC", "IPCC_2019")){
    GHG.ha.N2O.Direct.N2ON.F_SN <- In.F_SN * Ref.IPCC.EF1.F_SN
    names(GHG.ha.N2O.Direct.N2ON.F_SN) <- "GHG.ha.N2O.Direct.N2ON.F_SN"
    GHG.ha.N2O.Direct.N2ON.F_ON <- In.F_ON * Ref.IPCC.EF1.F_ON
    names(GHG.ha.N2O.Direct.N2ON.F_ON) <- "GHG.ha.N2O.Direct.N2ON.F_ON"
    GHG.ha.N2O.Direct.N2ON.F_CR <- In.F_CR * Ref.IPCC.EF1.F_CR
    names(GHG.ha.N2O.Direct.N2ON.F_CR) <- "GHG.ha.N2O.Direct.N2ON.F_CR"
  }
  if(N2OSimMethod %in% c("HUME","Measurement", "EF_Germany", "DNDC", "MODE")) {
    GHG.ha.N2O.Direct.N2ON.F_SN <- In.SimN2ON*In.F_SN/TotNInput
    names(GHG.ha.N2O.Direct.N2ON.F_SN) <- "GHG.ha.N2O.Direct.N2ON.F_SN"
    GHG.ha.N2O.Direct.N2ON.F_ON <- In.SimN2ON*In.F_ON/TotNInput
    names(GHG.ha.N2O.Direct.N2ON.F_ON) <- "GHG.ha.N2O.Direct.N2ON.F_ON"
    GHG.ha.N2O.Direct.N2ON.F_CR <- In.SimN2ON*In.F_CR/TotNInput
    names(GHG.ha.N2O.Direct.N2ON.F_CR) <- "GHG.ha.N2O.Direct.N2ON.F_CR"
  }
  if(N2OSimMethod== "GNOC"){
    GHG.ha.N2O.Direct.N2ON.F_SN <- In.SimN2ON*In.F_SN/TotNInput_F
    names(GHG.ha.N2O.Direct.N2ON.F_SN) <- "GHG.ha.N2O.Direct.N2ON.F_SN"
    GHG.ha.N2O.Direct.N2ON.F_ON <- In.SimN2ON*In.F_ON/TotNInput_F
    names(GHG.ha.N2O.Direct.N2ON.F_ON) <- "GHG.ha.N2O.Direct.N2ON.F_ON"
    GHG.ha.N2O.Direct.N2ON.F_CR <- In.F_CR * Ref.IPCC.EF1.F_CR
    names(GHG.ha.N2O.Direct.N2ON.F_CR) <- "GHG.ha.N2O.Direct.N2ON.F_CR"
  }

  # kg N2O/ha
  GHG.ha.N2O.Direct.N2O.F_SN <- GHG.ha.N2O.Direct.N2ON.F_SN * Ref.IPCC.ef.N2ON
  names(GHG.ha.N2O.Direct.N2O.F_SN) <- "GHG.ha.N2O.Direct.N2O.F_SN"
  GHG.ha.N2O.Direct.N2O.F_ON <- GHG.ha.N2O.Direct.N2ON.F_ON * Ref.IPCC.ef.N2ON
  names(GHG.ha.N2O.Direct.N2O.F_ON) <- "GHG.ha.N2O.Direct.N2O.F_ON"
  GHG.ha.N2O.Direct.N2O.F_CR <- GHG.ha.N2O.Direct.N2ON.F_CR * Ref.IPCC.ef.N2ON
  names(GHG.ha.N2O.Direct.N2O.F_CR) <- "GHG.ha.N2O.Direct.N2O.F_CR"

  # kg CO2eq/ha
  GHG.ha.N2O.Direct.CO2eq.F_SN <- GHG.ha.N2O.Direct.N2O.F_SN * Ref.RED.ef.N2O
  names(GHG.ha.N2O.Direct.CO2eq.F_SN) <- "GHG.ha.N2O.Direct.CO2eq.F_SN"
  GHG.ha.N2O.Direct.CO2eq.F_ON <- GHG.ha.N2O.Direct.N2O.F_ON * Ref.RED.ef.N2O
  names(GHG.ha.N2O.Direct.CO2eq.F_ON) <- "GHG.ha.N2O.Direct.CO2eq.F_ON"
  GHG.ha.N2O.Direct.CO2eq.F_CR <- GHG.ha.N2O.Direct.N2O.F_CR * Ref.RED.ef.N2O
  names(GHG.ha.N2O.Direct.CO2eq.F_CR) <- "GHG.ha.N2O.Direct.CO2eq.F_CR"


  ### Summe für Klimagasbilanz
  ### Auswahl welche berücksichtige werden sollen
  ### (Berechnungsweisen-Analyse)
  GHG.ha.N2O.Direct.N2ON <- Out.Ertrag.Feucht * 0 # kg N2O-N/ha
  GHG.ha.N2O.Direct.N2O <- Out.Ertrag.Feucht * 0 # kg N2O/ha
  GHG.ha.N2O.Direct.CO2eq <- Out.Ertrag.Feucht * 0 # kg C2Oeq/ha

  # Emissionen aus mineralischer N-Dgg
  # Ggf. mit Faktor
  if(IncludeSynthetic){
    GHG.ha.N2O.Direct.N2ON <- GHG.ha.N2O.Direct.N2ON + GHG.ha.N2O.Direct.N2ON.F_SN * as.numeric(IncludeSynthetic)
    GHG.ha.N2O.Direct.N2O <- GHG.ha.N2O.Direct.N2O + GHG.ha.N2O.Direct.N2O.F_SN * as.numeric(IncludeSynthetic)
    GHG.ha.N2O.Direct.CO2eq <- GHG.ha.N2O.Direct.CO2eq + GHG.ha.N2O.Direct.CO2eq.F_SN * as.numeric(IncludeSynthetic)
  }

  # Emissionen aus organischer N-Dgg
  # Ggf. mit Faktor
  if(IncludeOrganic){
    GHG.ha.N2O.Direct.N2ON <- GHG.ha.N2O.Direct.N2ON + GHG.ha.N2O.Direct.N2ON.F_ON * as.numeric(IncludeOrganic)
    GHG.ha.N2O.Direct.N2O <- GHG.ha.N2O.Direct.N2O + GHG.ha.N2O.Direct.N2O.F_ON * as.numeric(IncludeOrganic)
    GHG.ha.N2O.Direct.CO2eq <- GHG.ha.N2O.Direct.CO2eq + GHG.ha.N2O.Direct.CO2eq.F_ON * as.numeric(IncludeOrganic)
  }

  # Emissionen aus Ernteresiduen
  # Ggf. mit Faktor
  if(IncludeResidues){
    GHG.ha.N2O.Direct.N2ON <- GHG.ha.N2O.Direct.N2ON + GHG.ha.N2O.Direct.N2ON.F_CR * as.numeric(IncludeResidues)
    GHG.ha.N2O.Direct.N2O <- GHG.ha.N2O.Direct.N2O + GHG.ha.N2O.Direct.N2O.F_CR * as.numeric(IncludeResidues)
    GHG.ha.N2O.Direct.CO2eq <- GHG.ha.N2O.Direct.CO2eq + GHG.ha.N2O.Direct.CO2eq.F_CR * as.numeric(IncludeResidues)
  }

  names(GHG.ha.N2O.Direct.N2ON) <- "GHG.ha.N2O.Direct.N2ON"
  names(GHG.ha.N2O.Direct.N2O) <- "GHG.ha.N2O.Direct.N2O"
  names(GHG.ha.N2O.Direct.CO2eq) <- "GHG.ha.N2O.Direct.CO2eq"


  ### Indirekte N2O-Emission im Feld - Volatisation -  ####
  ### Berechnet nach IPCC 2006 Tier 1 ###

  ### Detailaufschlüsselung für Abb.  ###
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


  ### Summe für Klimagasbilanz
  ### Auswahl welche berücksichtige werden sollen
  ### (Berechnungsweisen-Analyse)
  GHG.ha.N2O.Vola.N2ON <- Out.Ertrag.Feucht * 0 # kg N2O-N/ha
  GHG.ha.N2O.Vola.N2O <- Out.Ertrag.Feucht * 0 # kg N2O/ha
  GHG.ha.N2O.Vola.CO2eq <- Out.Ertrag.Feucht * 0 # kg C2Oeq/ha

  # Emissionen aus mineralischer N-Dgg
  # Ggf. mit Faktor
  if(IncludeSynthetic){
    GHG.ha.N2O.Vola.N2ON <- GHG.ha.N2O.Vola.N2ON + GHG.ha.N2O.Vola.N2ON.F_SN * as.numeric(IncludeSynthetic)
    GHG.ha.N2O.Vola.N2O <- GHG.ha.N2O.Vola.N2O + GHG.ha.N2O.Vola.N2O.F_SN * as.numeric(IncludeSynthetic)
    GHG.ha.N2O.Vola.CO2eq <- GHG.ha.N2O.Vola.CO2eq + GHG.ha.N2O.Vola.CO2eq.F_SN * as.numeric(IncludeSynthetic)
  }

  # Emissionen aus organischer N-Dgg
  # Ggf. mit Faktor
  if(IncludeOrganic){
    GHG.ha.N2O.Vola.N2ON <- GHG.ha.N2O.Vola.N2ON + GHG.ha.N2O.Vola.N2ON.F_ON * as.numeric(IncludeOrganic)
    GHG.ha.N2O.Vola.N2O <- GHG.ha.N2O.Vola.N2O + GHG.ha.N2O.Vola.N2O.F_ON * as.numeric(IncludeOrganic)
    GHG.ha.N2O.Vola.CO2eq <- GHG.ha.N2O.Vola.CO2eq + GHG.ha.N2O.Vola.CO2eq.F_ON * as.numeric(IncludeOrganic)
  }

  names(GHG.ha.N2O.Vola.N2ON) <- "GHG.ha.N2O.Vola.N2ON"
  names(GHG.ha.N2O.Vola.N2O) <- "GHG.ha.N2O.Vola.N2O"
  names(GHG.ha.N2O.Vola.CO2eq) <- "GHG.ha.N2O.Vola.CO2eq"


  ### Indirekte N2O-Emission im Feld  ###
  ###       - Leaching/Runoff -       ###
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

  ### Summe für Klimagasbilanz
  ### Auswahl welche berücksichtige werden sollen
  ### (Berechnungsweisen-Analyse)
  GHG.ha.N2O.Leach.N2ON <- Out.Ertrag.Feucht * 0 # kg N2O-N/ha
  GHG.ha.N2O.Leach.N2O <- Out.Ertrag.Feucht * 0 # kg N2O/ha
  GHG.ha.N2O.Leach.CO2eq <- Out.Ertrag.Feucht * 0 # kg C2Oeq/ha

  # Emissionen aus mineralischer N-Dgg
  # Ggf. mit Faktor
  if(IncludeSynthetic){
    GHG.ha.N2O.Leach.N2ON <- GHG.ha.N2O.Leach.N2ON + GHG.ha.N2O.Leach.N2ON.F_SN * as.numeric(IncludeSynthetic)
    GHG.ha.N2O.Leach.N2O <- GHG.ha.N2O.Leach.N2O + GHG.ha.N2O.Leach.N2O.F_SN * as.numeric(IncludeSynthetic)
    GHG.ha.N2O.Leach.CO2eq <- GHG.ha.N2O.Leach.CO2eq + GHG.ha.N2O.Leach.CO2eq.F_SN * as.numeric(IncludeSynthetic)
  }

  # Emissionen aus organischer N-Dgg
  # Ggf. mit Faktor
  if(IncludeOrganic){
    GHG.ha.N2O.Leach.N2ON <- GHG.ha.N2O.Leach.N2ON + GHG.ha.N2O.Leach.N2ON.F_ON * as.numeric(IncludeOrganic)
    GHG.ha.N2O.Leach.N2O <- GHG.ha.N2O.Leach.N2O + GHG.ha.N2O.Leach.N2O.F_ON * as.numeric(IncludeOrganic)
    GHG.ha.N2O.Leach.CO2eq <- GHG.ha.N2O.Leach.CO2eq + GHG.ha.N2O.Leach.CO2eq.F_ON * as.numeric(IncludeOrganic)
  }

  # Emissionen aus Ernteresiduen
  # Ggf. mit Faktor
  if(IncludeResidues){
    GHG.ha.N2O.Leach.N2ON <- GHG.ha.N2O.Leach.N2ON + GHG.ha.N2O.Leach.N2ON.F_CR * as.numeric(IncludeResidues)
    GHG.ha.N2O.Leach.N2O <- GHG.ha.N2O.Leach.N2O + GHG.ha.N2O.Leach.N2O.F_CR * as.numeric(IncludeResidues)
    GHG.ha.N2O.Leach.CO2eq <- GHG.ha.N2O.Leach.CO2eq + GHG.ha.N2O.Leach.CO2eq.F_CR * as.numeric(IncludeResidues)
  }

  names(GHG.ha.N2O.Leach.N2ON) <- "GHG.ha.N2O.Leach.N2ON"
  names(GHG.ha.N2O.Leach.N2O) <- "GHG.ha.N2O.Leach.N2O"
  names(GHG.ha.N2O.Leach.CO2eq) <- "GHG.ha.N2O.Leach.CO2eq"


  ### Bezogen auf Ethanol- Output [g CO2eq/MJ Ethanol] ###
  GHG.MJ.N2O.Direct.N2ON.F_SN <- GHG.ha.N2O.Direct.N2ON.F_SN / Out.Ertrag.EthanolMJ_ha * 1000
  names(GHG.MJ.N2O.Direct.N2ON.F_SN) <- "GHG.MJ.N2O.Direct.N2ON.F_SN"
  GHG.MJ.N2O.Direct.N2ON.F_ON <- GHG.ha.N2O.Direct.N2ON.F_ON / Out.Ertrag.EthanolMJ_ha * 1000
  names(GHG.MJ.N2O.Direct.N2ON.F_ON) <- "GHG.MJ.N2O.Direct.N2ON.F_ON"
  GHG.MJ.N2O.Direct.N2ON.F_CR <- GHG.ha.N2O.Direct.N2ON.F_CR / Out.Ertrag.EthanolMJ_ha * 1000
  names(GHG.MJ.N2O.Direct.N2ON.F_CR) <- "GHG.MJ.N2O.Direct.N2ON.F_CR"
  GHG.MJ.N2O.Direct.N2O.F_SN <- GHG.ha.N2O.Direct.N2O.F_SN / Out.Ertrag.EthanolMJ_ha * 1000
  names(GHG.MJ.N2O.Direct.N2O.F_SN) <- "GHG.MJ.N2O.Direct.N2O.F_SN"
  GHG.MJ.N2O.Direct.N2O.F_ON <- GHG.ha.N2O.Direct.N2O.F_ON / Out.Ertrag.EthanolMJ_ha * 1000
  names(GHG.MJ.N2O.Direct.N2O.F_ON) <- "GHG.MJ.N2O.Direct.N2O.F_ON"
  GHG.MJ.N2O.Direct.N2O.F_CR <- GHG.ha.N2O.Direct.N2O.F_CR / Out.Ertrag.EthanolMJ_ha * 1000
  names(GHG.MJ.N2O.Direct.N2O.F_CR) <- "GHG.MJ.N2O.Direct.N2O.F_CR"
  GHG.MJ.N2O.Direct.CO2eq.F_SN <- GHG.ha.N2O.Direct.CO2eq.F_SN / Out.Ertrag.EthanolMJ_ha * 1000
  names(GHG.MJ.N2O.Direct.CO2eq.F_SN) <- "GHG.MJ.N2O.Direct.CO2eq.F_SN"
  GHG.MJ.N2O.Direct.CO2eq.F_ON <- GHG.ha.N2O.Direct.CO2eq.F_ON / Out.Ertrag.EthanolMJ_ha * 1000
  names(GHG.MJ.N2O.Direct.CO2eq.F_ON) <- "GHG.MJ.N2O.Direct.CO2eq.F_ON"
  GHG.MJ.N2O.Direct.CO2eq.F_CR <- GHG.ha.N2O.Direct.CO2eq.F_CR / Out.Ertrag.EthanolMJ_ha * 1000
  names(GHG.MJ.N2O.Direct.CO2eq.F_CR) <- "GHG.MJ.N2O.Direct.CO2eq.F_CR"
  GHG.MJ.N2O.Direct.N2ON <- GHG.ha.N2O.Direct.N2ON / Out.Ertrag.EthanolMJ_ha * 1000
  names(GHG.MJ.N2O.Direct.N2ON) <- "GHG.MJ.N2O.Direct.N2ON"
  GHG.MJ.N2O.Direct.N2O <- GHG.ha.N2O.Direct.N2O / Out.Ertrag.EthanolMJ_ha * 1000
  names(GHG.MJ.N2O.Direct.N2O) <- "GHG.MJ.N2O.Direct.N2O"
  GHG.MJ.N2O.Direct.CO2eq <- GHG.ha.N2O.Direct.CO2eq / Out.Ertrag.EthanolMJ_ha * 1000
  names(GHG.MJ.N2O.Direct.CO2eq) <- "GHG.MJ.N2O.Direct.CO2eq"
  GHG.MJ.N2O.Vola.N2ON.F_SN <- GHG.ha.N2O.Vola.N2ON.F_SN / Out.Ertrag.EthanolMJ_ha * 1000
  names(GHG.MJ.N2O.Vola.N2ON.F_SN) <- "GHG.MJ.N2O.Vola.N2ON.F_SN"
  GHG.MJ.N2O.Vola.N2ON.F_ON <- GHG.ha.N2O.Vola.N2ON.F_ON / Out.Ertrag.EthanolMJ_ha * 1000
  names(GHG.MJ.N2O.Vola.N2ON.F_ON) <- "GHG.MJ.N2O.Vola.N2ON.F_ON"
  GHG.MJ.N2O.Vola.N2O.F_SN <- GHG.ha.N2O.Vola.N2O.F_SN / Out.Ertrag.EthanolMJ_ha * 1000
  names(GHG.MJ.N2O.Vola.N2O.F_SN) <- "GHG.MJ.N2O.Vola.N2O.F_SN"
  GHG.MJ.N2O.Vola.N2O.F_ON <- GHG.ha.N2O.Vola.N2O.F_ON / Out.Ertrag.EthanolMJ_ha * 1000
  names(GHG.MJ.N2O.Vola.N2O.F_ON) <- "GHG.MJ.N2O.Vola.N2O.F_ON"
  GHG.MJ.N2O.Vola.CO2eq.F_SN <- GHG.ha.N2O.Vola.CO2eq.F_SN / Out.Ertrag.EthanolMJ_ha * 1000
  names(GHG.MJ.N2O.Vola.CO2eq.F_SN) <- "GHG.MJ.N2O.Vola.CO2eq.F_SN"
  GHG.MJ.N2O.Vola.CO2eq.F_ON <- GHG.ha.N2O.Vola.CO2eq.F_ON / Out.Ertrag.EthanolMJ_ha * 1000
  names(GHG.MJ.N2O.Vola.CO2eq.F_ON) <- "GHG.MJ.N2O.Vola.CO2eq.F_ON"
  GHG.MJ.N2O.Vola.N2ON <- GHG.ha.N2O.Vola.N2ON / Out.Ertrag.EthanolMJ_ha * 1000
  names(GHG.MJ.N2O.Vola.N2ON) <- "GHG.MJ.N2O.Vola.N2ON"
  GHG.MJ.N2O.Vola.N2O <- GHG.ha.N2O.Vola.N2O / Out.Ertrag.EthanolMJ_ha * 1000
  names(GHG.MJ.N2O.Vola.N2O) <- "GHG.MJ.N2O.Vola.N2O"
  GHG.MJ.N2O.Vola.CO2eq <- GHG.ha.N2O.Vola.CO2eq / Out.Ertrag.EthanolMJ_ha * 1000
  names(GHG.MJ.N2O.Vola.CO2eq) <- "GHG.MJ.N2O.Vola.CO2eq"
  GHG.MJ.N2O.Leach.N2ON.F_SN <- GHG.ha.N2O.Leach.N2ON.F_SN / Out.Ertrag.EthanolMJ_ha * 1000
  names(GHG.MJ.N2O.Leach.N2ON.F_SN) <- "GHG.MJ.N2O.Leach.N2ON.F_SN"
  GHG.MJ.N2O.Leach.N2ON.F_ON <- GHG.ha.N2O.Leach.N2ON.F_ON / Out.Ertrag.EthanolMJ_ha * 1000
  names(GHG.MJ.N2O.Leach.N2ON.F_ON) <- "GHG.MJ.N2O.Leach.N2ON.F_ON"
  GHG.MJ.N2O.Leach.N2ON.F_CR <- GHG.ha.N2O.Leach.N2ON.F_CR / Out.Ertrag.EthanolMJ_ha * 1000
  names(GHG.MJ.N2O.Leach.N2ON.F_CR) <- "GHG.MJ.N2O.Leach.N2ON.F_CR"
  GHG.MJ.N2O.Leach.N2O.F_SN <- GHG.ha.N2O.Leach.N2O.F_SN / Out.Ertrag.EthanolMJ_ha * 1000
  names(GHG.MJ.N2O.Leach.N2O.F_SN) <- "GHG.MJ.N2O.Leach.N2O.F_SN"
  GHG.MJ.N2O.Leach.N2O.F_ON <- GHG.ha.N2O.Leach.N2O.F_ON / Out.Ertrag.EthanolMJ_ha * 1000
  names(GHG.MJ.N2O.Leach.N2O.F_ON) <- "GHG.MJ.N2O.Leach.N2O.F_ON"
  GHG.MJ.N2O.Leach.N2O.F_CR <- GHG.ha.N2O.Leach.N2O.F_CR / Out.Ertrag.EthanolMJ_ha * 1000
  names(GHG.MJ.N2O.Leach.N2O.F_CR) <- "GHG.MJ.N2O.Leach.N2O.F_CR"
  GHG.MJ.N2O.Leach.CO2eq.F_SN <- GHG.ha.N2O.Leach.CO2eq.F_SN / Out.Ertrag.EthanolMJ_ha * 1000
  names(GHG.MJ.N2O.Leach.CO2eq.F_SN) <- "GHG.MJ.N2O.Leach.CO2eq.F_SN"
  GHG.MJ.N2O.Leach.CO2eq.F_ON <- GHG.ha.N2O.Leach.CO2eq.F_ON / Out.Ertrag.EthanolMJ_ha * 1000
  names(GHG.MJ.N2O.Leach.CO2eq.F_ON) <- "GHG.MJ.N2O.Leach.CO2eq.F_ON"
  GHG.MJ.N2O.Leach.CO2eq.F_CR <- GHG.ha.N2O.Leach.CO2eq.F_CR / Out.Ertrag.EthanolMJ_ha * 1000
  names(GHG.MJ.N2O.Leach.CO2eq.F_CR) <- "GHG.MJ.N2O.Leach.CO2eq.F_CR"
  GHG.MJ.N2O.Leach.N2ON <- GHG.ha.N2O.Leach.N2ON / Out.Ertrag.EthanolMJ_ha * 1000
  names(GHG.MJ.N2O.Leach.N2ON) <- "GHG.MJ.N2O.Leach.N2ON"
  GHG.MJ.N2O.Leach.N2O <- GHG.ha.N2O.Leach.N2O / Out.Ertrag.EthanolMJ_ha * 1000
  names(GHG.MJ.N2O.Leach.N2O) <- "GHG.MJ.N2O.Leach.N2O"
  GHG.MJ.N2O.Leach.CO2eq <- GHG.ha.N2O.Leach.CO2eq / Out.Ertrag.EthanolMJ_ha * 1000
  names(GHG.MJ.N2O.Leach.CO2eq) <- "GHG.MJ.N2O.Leach.CO2eq"

  # Summen
  GHG.ha.N2O.Summe.N2ON <- GHG.ha.N2O.Direct.N2ON
  if(IncludeVolatil){
    GHG.ha.N2O.Summe.N2ON <- GHG.ha.N2O.Summe.N2ON + GHG.ha.N2O.Vola.N2ON}
  # if(IncludeVolatil){
  #   GHG.ha.N2O.Summe.N2ON <- GHG.ha.N2O.Summe.N2ON + GHG.ha.N2O.Leach.N2ON}
  if(IncludeLeaching){
    GHG.ha.N2O.Summe.N2ON <- GHG.ha.N2O.Summe.N2ON + GHG.ha.N2O.Leach.N2ON}
  names(GHG.ha.N2O.Summe.N2ON) <- "GHG.ha.N2O.Summe.N2ON"

  GHG.ha.N2O.Summe.N2O <- GHG.ha.N2O.Direct.N2O
  if(IncludeVolatil){
    GHG.ha.N2O.Summe.N2O <- GHG.ha.N2O.Summe.N2O + GHG.ha.N2O.Vola.N2O}
  if(IncludeLeaching){
    GHG.ha.N2O.Summe.N2O <- GHG.ha.N2O.Summe.N2O + GHG.ha.N2O.Leach.N2O}
  names(GHG.ha.N2O.Summe.N2O) <- "GHG.ha.N2O.Summe.N2O"
  GHG.ha.N2O.Summe.CO2eq <- GHG.ha.N2O.Direct.CO2eq
  if(IncludeVolatil){
    GHG.ha.N2O.Summe.CO2eq <- GHG.ha.N2O.Summe.CO2eq + GHG.ha.N2O.Vola.CO2eq}
  if(IncludeLeaching){
    GHG.ha.N2O.Summe.CO2eq <- GHG.ha.N2O.Summe.CO2eq + GHG.ha.N2O.Leach.CO2eq}
  names(GHG.ha.N2O.Summe.CO2eq) <- "GHG.ha.N2O.Summe.CO2eq"
  GHG.MJ.N2O.Summe.N2ON <- GHG.ha.N2O.Summe.N2ON / Out.Ertrag.EthanolMJ_ha * 1000
  names(GHG.MJ.N2O.Summe.N2ON) <- "GHG.MJ.N2O.Summe.N2ON"
  GHG.MJ.N2O.Summe.N2O <- GHG.ha.N2O.Summe.N2O / Out.Ertrag.EthanolMJ_ha * 1000
  names(GHG.MJ.N2O.Summe.N2O) <- "GHG.MJ.N2O.Summe.N2O"
  GHG.MJ.N2O.Summe.CO2eq <- GHG.ha.N2O.Summe.CO2eq / Out.Ertrag.EthanolMJ_ha * 1000
  names(GHG.MJ.N2O.Summe.CO2eq) <- "GHG.MJ.N2O.Summe.CO2eq"


  ### Klimagasemissionen Bereitstellung ###
  ###      Dünger & Betriebsmittel      ###
  # kg CO2eq/ha
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
  GHG.ha.PSM <- In.PSM * Ref.BG.ef.PSM
  names(GHG.ha.PSM) <- "GHG.ha.PSM"
  GHG.ha.Saatgut <- In.Saatgut * Ref.BG.ef.Saatgut
  names(GHG.ha.Saatgut) <- "GHG.ha.Saatgut"
  GHG.ha.Field.acidification <- In.Field.acidification                                                                       # ist schon in kg/ha Co2
  names (GHG.ha.Field.acidification) <- "GHG.ha.Field.acidification"
  GHG.ha.Diesel <- In.Diesel * (Ref.BG.ef.Diesel.l + Ref.BG.ef.Diesel.use.CH4.N2O)                                             # neu in BGII
  names(GHG.ha.Diesel) <- "GHG.ha.Diesel"

  GHG.ha.OtherEm_MJ <- GHG.MJ.OtherEm_MJ * Out.Ertrag.EthanolMJ_ha / 1000
  names(GHG.ha.OtherEm_MJ) <- "GHG.ha.OtherEm_MJ"

  GHG.ha.Drying <- In.Dry.Therm_MJ * Ref.BG.ef.NaturalGas + In.Dry.Electric * Ref.BG.ef.Electricity
  names(GHG.ha.Drying) <- "GHG.ha.Drying"
  GHG.ha.Irrigation <- In.Irri.Amount_mm*In.Irri.EnergyUseMJ_mm* In.ef.Irri.Energy
  names(GHG.ha.Irrigation) <- "GHG.ha.Irrigation"

  GHG.ha.Summe <- (GHG.ha.NDuenger + GHG.ha.N2O + GHG.ha.CaCO3Duenger +
                     GHG.ha.K2ODuenger + GHG.ha.P2O5Duenger + GHG.ha.PSM +
                     GHG.ha.Saatgut + GHG.ha.Diesel+ GHG.ha.Field.acidification + GHG.ha.OtherEm_ha + GHG.ha.OtherEm_MJ + GHG.ha.Irrigation)

  if(IncludeDrying){
    GHG.ha.Summe <- GHG.ha.Summe + GHG.ha.Drying
  }
  names(GHG.ha.Summe) <- "GHG.ha.Summe"


  # g CO2eq/MJ Ethanl
  GHG.MJ.NDuenger <- GHG.ha.NDuenger / Out.Ertrag.EthanolMJ_ha * 1000
  names(GHG.MJ.NDuenger) <- "GHG.MJ.NDuenger"
  GHG.MJ.N2O <- GHG.ha.N2O / Out.Ertrag.EthanolMJ_ha * 1000
  names(GHG.MJ.N2O) <- "GHG.MJ.N2O"
  GHG.MJ.CaCO3Duenger <- GHG.ha.CaCO3Duenger / Out.Ertrag.EthanolMJ_ha * 1000
  names(GHG.MJ.CaCO3Duenger) <- "GHG.MJ.CaCO3Duenger"
  GHG.MJ.K2ODuenger <- GHG.ha.K2ODuenger / Out.Ertrag.EthanolMJ_ha * 1000
  names(GHG.MJ.K2ODuenger) <- "GHG.MJ.K2ODuenger"
  GHG.MJ.P2O5Duenger <- GHG.ha.P2O5Duenger / Out.Ertrag.EthanolMJ_ha * 1000
  names(GHG.MJ.P2O5Duenger) <- "GHG.MJ.P2O5Duenger"
  GHG.MJ.PSM <- GHG.ha.PSM / Out.Ertrag.EthanolMJ_ha * 1000
  names(GHG.MJ.PSM) <- "GHG.MJ.PSM"
  GHG.MJ.Saatgut <- GHG.ha.Saatgut / Out.Ertrag.EthanolMJ_ha * 1000
  names(GHG.MJ.Saatgut) <- "GHG.MJ.Saatgut"
  GHG.MJ.Field.acidification <- GHG.ha.Field.acidification / Out.Ertrag.EthanolMJ_ha * 1000
  names (GHG.MJ.Field.acidification) <- "GHG.MJ.Field.acidification"
  GHG.MJ.Diesel <- GHG.ha.Diesel / Out.Ertrag.EthanolMJ_ha * 1000
  names(GHG.MJ.Diesel) <- "GHG.MJ.Diesel"
  GHG.MJ.OtherEm_ha <- GHG.ha.OtherEm_ha / Out.Ertrag.EthanolMJ_ha * 1000
  names(GHG.MJ.OtherEm_ha) <- "GHG.MJ.OtherEm_ha"
  GHG.MJ.Drying <- GHG.ha.Drying / Out.Ertrag.EthanolMJ_ha * 1000
  names(GHG.MJ.Drying) <- "GHG.MJ.Drying"
  GHG.MJ.Irrigation <- GHG.ha.Irrigation / Out.Ertrag.EthanolMJ_ha * 1000
  names(GHG.MJ.Irrigation) <- "GHG.MJ.Irrigation"
  GHG.MJ.Summe <- (GHG.MJ.NDuenger + GHG.MJ.N2O + GHG.MJ.CaCO3Duenger +
                     GHG.MJ.K2ODuenger + GHG.MJ.P2O5Duenger + GHG.MJ.PSM +
                     GHG.MJ.Saatgut + GHG.MJ.Field.acidification + GHG.MJ.Diesel + GHG.MJ.OtherEm_ha + GHG.MJ.OtherEm_MJ + GHG.MJ.Irrigation)
  if(IncludeDrying){
    GHG.MJ.Summe <- GHG.MJ.Summe + GHG.MJ.Drying
  }
  names(GHG.MJ.Summe) <- "GHG.MJ.Summe"

  # Emissionen Anbau
  # NICHT Alloziert
  GHG.eec_nonallo <- GHG.MJ.Summe
  names(GHG.eec_nonallo) <- "GHG.eec_nonallo"

  # Emissionen Anbau
  # Alloziert
  GHG.eec <- GHG.eec_nonallo * Ref.BG.allocation
  names(GHG.eec) <- "GHG.eec"

  # Emissionen Biokraftstoff
  # Alloziert
  GHG.EB <- GHG.eec + GHG.ep + GHG.etd
  names(GHG.EB) <- "GHG.EB"

  # Einsparpotential
  GHG.Savings <- (Ref.RED.ef.gasoline - GHG.EB) / Ref.RED.ef.gasoline *100
  names(GHG.Savings) <- "GHG.Savings"


  ###                   ###
  ### Output generieren ###
  ###                   ###

  for (i in 1:length(Show)) {
    if(!(Show[[i]] %in% ValidArg.Show)){
      print(paste("WARNUNG:", Show[[i]],
                  "ist kein gueltiges Argument fuer die Ausgabe"))
    }
  }
  # In.F_CR <- as.numeric(unlist(In.F_CR))

  # print(names(In.F_SN))
  # print(names(In.F_CR))
  # print(str(In.F_SN))
  # print(str(In.F_CR))

  OutputFrame <- list(Out.Ertrag.Feucht, Out.Feuchte, Out.Ertrag.13.5prz,
                      Out.Ertrag.DM, Out.Ertrag.WeizenMJ_ha, Out.Ertrag.EthanolMJ_ha,
                      In.F_SN, In.F_ON, In.F_CR, In.CaCO3, In.K2O, In.P2O5, In.PSM,
                      In.Saatgut,In.Field.acidification, In.Diesel, In.Dry.Therm_MJ, In.Dry.Electric,
                      In.Irri.Amount_mm,In.Irri.EnergyUseMJ_mm, In.ef.Irri.Energy,
                      GHG.MJ.OtherEm_ha, GHG.MJ.OtherEm_MJ, GHG.ha.OtherEm_ha, GHG.ha.OtherEm_MJ,
                      GHG.eec_nonallo, GHG.eec, GHG.ep, GHG.etd, GHG.EB, GHG.Savings)

  if(("complete" %in% Show) | ("eec" %in% Show) | ("eec.ha" %in% Show)){
    OutputFrame <- list(as.data.frame(OutputFrame),
                        GHG.ha.NDuenger, GHG.ha.CaCO3Duenger, GHG.ha.K2ODuenger,
                        GHG.ha.P2O5Duenger, GHG.ha.PSM, GHG.ha.Saatgut, GHG.ha.N2O, GHG.ha.Field.acidification,
                        GHG.ha.Diesel, GHG.ha.Drying, GHG.ha.Irrigation, GHG.ha.Summe)
  }

  if(("complete" %in% Show) | ("eec" %in% Show) | ("eec.MJ" %in% Show)){
    OutputFrame <- list(as.data.frame(OutputFrame),
                        GHG.MJ.NDuenger, GHG.MJ.CaCO3Duenger, GHG.MJ.K2ODuenger,
                        GHG.MJ.P2O5Duenger, GHG.MJ.PSM, GHG.MJ.Saatgut, GHG.MJ.N2O, GHG.MJ.Field.acidification,
                        GHG.MJ.Diesel, GHG.MJ.Drying, GHG.MJ.Irrigation, GHG.MJ.Summe)
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
                        In.F_CR.DueV,  In.F_CR.IPCC, Out.DueV.BiomassAG,
                        Out.DueV.BiomassAGN, Out.DueV.NHarvest,
                        Out.IPCC.BiomassAG)
  }

  OutputFrame <- list(Treatment, as.data.frame(OutputFrame))
  DF <- as.data.frame(OutputFrame)
  DF
  # write.table(DF, "X:\\rabah_nasser\\Dataset_GHG_R\\GWP_273\\output_Weizen_JRC_neu_HUME.vals", quote = FALSE, row.names = FALSE, sep = "\t")
}


