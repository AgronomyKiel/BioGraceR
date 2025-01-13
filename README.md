
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RBioGraceII

<!-- badges: start -->
<!-- badges: end -->

The goal of RBioGraceII is to calculate the greenhouse gas emissions of
biofuels. The package is based on the BioGraceII software, which is a
tool for calculating the greenhouse gas emissions of biofuels. The code
is based on the Excel-Tool Biograce
(<https://www.biograce.net/biograce2/>) implementing standards of the
Renewable Energy Directive (RED).

# Installation

You can install the development version of RBioGraceII from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("HenningKage/BioGraceR")
```

# Input

| Parameter | meaning |
|----|----|
| Ertrag | seed yield in (kg/ha) give as fresh dry matter |
| Treatment | Optional Vector with Treatment/Variant-Codes |
| Feuchte | water content of OSR seed given as (%), default value is 9% |
| Oil_9 | Oil content of OSR seed given as (%), at 9% moisture, default is 42.6 |
| Nmineral | mineral N-fertilisation in (kg N/ha), default value is 142 kg N/ha |
| SimNLeach | Optional direct quantification of N-Leaching in (kg N/ha), by using own estimates or from simulation modelling |
| SimN<sub>2</sub>ON | Optional direct quantification of direct N<sub>2</sub>O-N Emissions in (kg N/ha), by using own estimates or from simulation modelling |
| SimResidue | Optional direct quantification of N-amount in Crop residues in (kg N/ha), by using own estimates or from simulation modelling |
| sim | logical (vector) for using own estimates/simulated input values |
| Norgan | organic N fertilization in (kg/ha) |
| P2O5 | Optional fertilisation rate of phosphorous in (kg P2O5/ha), if not given, fertilisation is assumed to replace P offtake by seeds |
| K2O | Optional fertilisation rate of potassium in (kg K2O/ha), if not given fertilisation is assumed to replace K offtake by seeds |
| CaCO<sub>3</sub> | Optional rate of liming in (kg CaCO<sub>3</sub>/ha) |
| PSM | Input of plant protection products in (kg/ha) |
| Saatgut | seed input (kg/ha) |
| Diesel | Diesel use for field operations default value is 83.3 l/ha |
| Field.acidification | needed?? |
| ep | emissions from production of biodiesel (g CO<sub>2</sub>eq/MJ), default is 11.7 g CO<sub>2</sub>eq/MJ, (typical value - REDII) |
| etd | emissions for transport in the production chain in (g CO<sub>2</sub>eq/MJ) |
| ResidueCalc | Option for calculation of N from crop residues choices: “Chalon”, “IPCC”, “DueV”, “BioGrace”, “SimValue” |
| N<sub>2</sub>OCalcMethod | Tier 1 uses aggregated emission factors for disaggretated factors use option “custom” |
| IncludeOrganic | inclusion of organic fertiliser N in emission calculation (T/F, yes, no) |
| IncludeResidues | inclusion of crop residues N in emission calculation (T/F, yes, no) |
| IncludeSynthetic | inclusion of synthetic N in emission calculation (T/F, yes, no) |
| N<sub>2</sub>OSimMethod | Options for calculation of direct N<sub>2</sub>O-emissions “IPCC”, \# “HUME ,GNOC, DNDC und MODE” SimN<sub>2</sub>ON nicht 0 |
| IncludeDrying | include emissions due to seed drying |
| IncludeVolatil | option (T/F) for inclusion (exclusion) of gaseous emission, default is inclusion T |
| IncludeLeaching | option (T/F) for inclusion (exclusion) of leaching based emission, default is inclusion T |
| OtherEm_ha | other emission in kg CO<sub>2</sub>eq/ha |
| OtherEm_MJ | other emissions in MJ |
| Dis.EF1.F_SN | Synthetic fertilizer inputs in wet climates IPCC(2019) 0.019 is default value |
| Dis.EF1.F_ON | Other N inputs in wet climates IPCC (2019) 0.006 is default |
| Dis.EF1.F_CR | disaggretated emission factor for N input from crop residues, 0.006 is default |
| Dis.IPCC.EF4 | emission factor for N<sub>2</sub>O emissions from atmospheric deposition of N on soils and water surfaces |
| Dis.CAS.Frac_GASF | disaggregated ammonia emission factor (Volatilization from synthetic fertilizer), 0.05 is default (ammonia-nitrate) |
| Frac_NLeach | Leaching fraction of N-input, default value is 0.24 according to IPCC 2019 |
| EF.SN.Prod | Emission for production of synthetic N fertilizer in kg CO<sub>2</sub>eq/kg N, default is 3.469 for calcium-ammonium nitrate from additinal standard values of BioGraceII |
| Show | Options for output |

# Output

## Standard ouput

Output-Parameter GHGCalc, Version 2_0, 19.05.2011

# Bilanzierungsergebnisse

Ausgabe mit: - (wird immer ausgegeben)

| Name | Bedeutung | Einheit |
|----|----|----|
| Out.Ertrag.Feucht | Ertrag; feldfeucht | \[kg/ha\] |
| Out.Feuchte | Samenfeuchte bei Ernte | \[kg H<sub>2</sub>O/kg Raps\] |
| Out.Ertrag.9prz | Ertrag; standardisiert auf 9% H<sub>2</sub>O | \[kg/ha\] |
| Out.Ertrag.OM | Ertrag; Trockensubstanz | \[kg DM/ha\] |
| Out.Ertrag.RapsMJ_ha | Energieertrag, reiner Raps, ohne Konversion | \[MJ/ha\] |
| Out.Ertrag.RMEMJ_ha | RME-Ertrag je ha | \[MJ RME/ha\] |
| In.F_SN | Aufwandmenge mineralischer N-Dünger | \[kg N/ha\] |
| In.F_ON | Aufwandmenge organischer N-Dünger | \[kg N/ha\] |
| In.F_GR | N in Ernteresiduen wie in Bilanz berucksichtigt | \[kg N/ha\] |
| In.CaO | Aufwandmenge Düngerkalk | \[kg CaO/ha\] |
| In.K<sub>2</sub>O | Aufwandmenge K-Dünger | \[kg K<sub>2</sub>O/ha\] |
| In.P<sub>2</sub>O<sub>5</sub> | Aufwandmenge P-Dünger | \[kg P<sub>2</sub>O<sub>5</sub>/ha\] |
| In.PSM | Aufwandmenge Pflanzenschutzmittel | \[kg AI/ha\] |
| In.Saatgut | Aufwandmenge Saatgut | \[kg Rapssaat/ha\] |
| In.Diesel | Verbrauchter Diesel | \[l Diesel/ha\] |
| In.Dry.Therm_MJ | Benotigte Thermische Energie zur Trocknung | \[MJ/ha\] |
| In.Dry.Electric | Benotigte elektrische Energie zu Trocknung | \[MJ/ha\] |
| GHG.eec_nonallo | THG-Emissionen Anbau Biomasse (NICHT alloziert) | \[g CO<sub>2</sub>eq/MJ RME\] |
| GHG.eec | THG-Emissionen Anbau Biomasse (alloziert) | \[g CO<sub>2</sub>eq/MJ RME\] |
| GHG.ep | THG-Emissionen Produktion Biokraftstoff aus Biomasse (alloziert) | \[g CO<sub>2</sub>eq/MJ RME\] |
| GHG.etd | THG-Emissionen fur Transporte (alloziert) | \[g CO<sub>2</sub>eq/MJ RME\] |
| GHG.EB | THG-Emissionen fur Bereitstellung Biokraftstoff (alloziert) | \[g CO<sub>2</sub>eq/MJ RME\] |
| GHG.Savings | THG-Minderungspotential | \[%\] |

# THG-Emissionen aus Pflanzenbau

## Teilaggregiert, Fläche

\# Ausgabe mit complete, eec, eec.ha

| Name | Bedeutung | Einheit |
|----|----|----|
| GHG.ha.NDuenger | THG-Emissionen Bereitstellung N-Dünger (NICHT alloziert) | \[kg CO<sub>2</sub>eq/ha\] |
| GHG.ha.CaODuenger | THG-Emissionen Bereitstellung CaO-Dünger (NICHT alloziert) | \[kg CO<sub>2</sub>eq/ha\] |
| GHG.ha.K<sub>2</sub>ODuenger | THG-Emissionen Bereitstellung K<sub>2</sub>O-Dünger (NICHT alloziert) | \[kg CO<sub>2</sub>eq/ha\] |
| GHG.ha.P<sub>2</sub>O<sub>5</sub>Duenger | THG-Emissionen Bereitstellung P-Dünger (NICHT alloziert) | \[kg CO<sub>2</sub>eq/ha\] |
| GHG.ha.PSM | THG-Emissionen Bereitstellung van PSM (NICHT alloziert) | \[kg CO<sub>2</sub>eq/ha\] |
| GHG.ha.Saatgut | THG-Emissionen Bereitstellung des Saatguts (NICHT alloziert) | \[kg CO<sub>2</sub>eq/ha\] |
| GHG.ha.N20 | THG-Emissionen durch Lachgasemissionen auf dem Feld (NICHT alloziert) | \[kg CO<sub>2</sub>eq/ha\] |
| GHG.ha.Diesel | THG-Emissionen Bereitstellung & Verw. Diesel (NICHT alloziert) | \[kg CO<sub>2</sub>eq/ha\] |
| GHG.ha.Drying | THG-Emissionen Trocknung (NICHT alloziert) | \[kg CO<sub>2</sub>eq/ha\] |
| GHG.ha.Other | THG-Emissionen; Sonderposten fur weiteres oder Offset-Verschiebung (NICHT alloziert) | \[kg CO<sub>2</sub>eq/ha\] |
| GHG.ha.Summe | THG-Emissionen Anbau der Biomasse (NICHT alloziert) | \[kg CO<sub>2</sub>eq/ha\] |
| GHG.ha.Diesel | THG-Emissionen Bereitstellung & Verw. Diesel (NICHT alloziert) | \[kg CO<sub>2</sub>eq/ha\] |
| GHG.ha.Drying | THG-Emissionen Trocknung (NICHT alloziert) | \[kg CO<sub>2</sub>eq/ha\] |
| GHG.ha.Other | THG-Emissionen; Sonderposten fur weiteres oder Offset-Verschiebung (NICHT alloziert) | \[kg CO<sub>2</sub>eq/ha\] |
| GHG.ha.Summe | THG-Emissionen Anbau der Biomasse (NICHT alloziert) | \[kg CO<sub>2</sub>eq/ha\] |

# THG-Emissionen aus Pflanzenbau

## Teilaggregiert, Endprodukt

Ausgabe mit complete, eec, eec.MJ

| Name | Bedeutung | Einheit |
|----|----|----|
| GHG.MJ.NDuenger | THG-Emissionen Bereitstellung N-Dünger (NICHT alloziert) | \[g CO<sub>2</sub>eq/MJ RME\] |
| GHG.MJ.CaODuenger | THG-Emissionen Bereitstellung CaO-Dünger (NICHT alloziert) | \[g CO<sub>2</sub>eq/MJ RME\] |
| GHG.MJ.K<sub>2</sub>ODuenger | THG-Emissionen Bereitstellung K<sub>2</sub>O-Dünger (NICHT alloziert) | \[g CO<sub>2</sub>eq/MJ RME\] |
| GHG.MJ.P<sub>2</sub>O<sub>5</sub>Duenger | THG-Emissionen Bereitstellung P-Dünger (NICHT alloziert) | \[g CO<sub>2</sub>eq/MJ RME\] |
| GHG.MJ.PSM | THG-Emissionen Bereitstellung von PSM (NICHT alloziert) | \[g CO<sub>2</sub>eq/MJ RME\] |
| GHG.MJ.Saatgut | THG-Emissionen Bereitstellung des Saatguts (NICHT alloziert) | \[g CO<sub>2</sub>eq/MJ RME\] |
| GHG.MJ.N<sub>2</sub>O | THG-Emissionen durch Lachgasemissionen auf dem Feld (NICHT alloziert) | \[g CO<sub>2</sub>eq/MJ RME\] |
| GHG.MJ.Diesel | THG-Emissionen Bereitstellung & Verw. Diesel (NICHT alloziert) | \[g CO<sub>2</sub>eq/MJ RME\] |
| GHG.MJ.Drying | THG-Emissionen Trocknung (NICHT alloziert) | \[g CO<sub>2</sub>eq/MJ RME\] |
| GHG.MJ.Other | THG-Emissionen; Sonderposten für weiteres oder Offset-Verschiebung (NICHT alloziert) | \[g CO<sub>2</sub>eq/MJ RME\] |
| GHG.MJ.Summe | THG-Emissionen Anbau der Biomasse (NICHT alloziert) | \[g CO<sub>2</sub>eq/MJ RME\] (==GHG.eec_nonallo) |

# Lachgasemissionen

## Aggregiert

\# Ausgabe mit complete, N<sub>2</sub>O, N<sub>2</sub>O.detail.ha,
N<sub>2</sub>O.detail.MJ, N<sub>2</sub>O.detail.ha.more,
N<sub>2</sub>O.detail.MJ.more

| Name | Bedeutung | Einheit |
|----|----|----|
| GHG.ha.N<sub>2</sub>O.Summe.N<sub>2</sub>ON | Lachgasemissionen | \[kg N<sub>2</sub>O-N/ha\] |
| GHG.ha.N<sub>2</sub>O.Summe.N<sub>2</sub>O | Lachgasemissionen | \[kg N<sub>2</sub>O/ha\] |
| GHG.ha.N<sub>2</sub>O.Summe.CO<sub>2</sub>eq | Lachgasemissionen | \[kg CO<sub>2</sub>eq/ha\] |
| GHG.MJ.N<sub>2</sub>O.Summe.N<sub>2</sub>ON | Lachgasemissionen | \[g N<sub>2</sub>O-N/MJ RME\] |
| GHG.MJ.N<sub>2</sub>O.Summe.N<sub>2</sub>O | Lachgasemissionen | \[g N<sub>2</sub>O/MJ RME\] |
| GHG.MJ.N<sub>2</sub>O.Summe.CO<sub>2</sub>eq | Lachgasemissionen | \[g CO<sub>2</sub>eq/MJ RME\] |

## Teilaggregiert, Fläche

\# Ausgabe *mit* complete, N<sub>2</sub>O.detail.ha,
N<sub>2</sub>O.detail.ha.more

| Name | Bedeutung | Einheit |
|----|----|----|
| GHG.ha.N<sub>2</sub>O.Direct.N<sub>2</sub>O | Direkte Lachgasemissionen | \[kg N<sub>2</sub>O/ha\] |
| GHG.ha.N<sub>2</sub>O.Direct.CO<sub>2</sub>eq | Direkte Lachgasemissionen | \[kg CO<sub>2</sub>eq/ha\] |
| GHG.ha.N<sub>2</sub>O.Vola.N<sub>2</sub>ON | Indirekte (volatilisation)Lachgasemissionen | \[kg N<sub>2</sub>O-N/ha\] |
| GHG.ha.N<sub>2</sub>O.Vola.N<sub>2</sub>O | Indirekte (volatilisation)Lachgasemissionen | \[kg N<sub>2</sub>O/ha\] |
| GHG.ha.N<sub>2</sub>O.Vola.CO<sub>2</sub>eq | Indirekte (volatilisation)Lachgasemissionen | \[kg |
| CO<sub>2</sub>eq/ha\] GHG.ha.N<sub>2</sub>O.Leach.N<sub>2</sub>ON | Indirekte (leaching)Lachgasemissionen | \[kg N<sub>2</sub>O-/ha\] |
| GHG.ha.N<sub>2</sub>O.Leach.N<sub>2</sub>O | Indirekte (leaching) Lachgasemissionen | \[kg N<sub>2</sub>O/ha\] |
| GHG.ha.N<sub>2</sub>O.Leach.CO<sub>2</sub>eq | Indirekte (leaching)Lachgasemissionen | \[kg CO<sub>2</sub>eq/ha\] |

## Detailiert, Fläche

\# Ausgabe mit complete, N<sub>2</sub>O.detail.ha.more

| Name | Bedeutung | Einheit |
|----|----|----|
| GHG.ha.N<sub>2</sub>O.Direct.N<sub>2</sub>ON.F_SN | Direkte Lachgasemissionen aus mineralischer Düngung | \[kg N<sub>2</sub>O-N/ha\] |
| GHG.ha.N<sub>2</sub>O.Direct.N<sub>2</sub>ON.F_ON | Direkte Lachgasemissionen aus organischer Düngung | \[kg N<sub>2</sub>O-N/ha\] |
| GHG.ha.N<sub>2</sub>O.Direct.N<sub>2</sub>ON.F_CR | Direkte Lachgasemissionen aus Ernterückständen | \[kg N<sub>2</sub>O-N/ha\] |
| GHG.ha.N<sub>2</sub>O.Direct.N<sub>2</sub>O.F_SN | Direkte Lachgasemissionen aus mineralischer Düngung | \[kg N<sub>2</sub>O/ha\] |
| GHG.ha.N<sub>2</sub>O.Direct.N<sub>2</sub>O.F_ON | Direkte Lachgasemissionen aus organischer Düngung | \[kg N<sub>2</sub>O/ha\] |
| GHG.ha.N<sub>2</sub>O.Direct.N<sub>2</sub>O.F_CR | Direkte Lachgasemissionen aus Ernterockstanden | \[kg N<sub>2</sub>O/ha\] |
| GHG.ha.N<sub>2</sub>O.Direct.CO<sub>2</sub>eq.F_SN | Direkte Lachgasemissionen aus mineralischer Düngung | \[kg CO<sub>2</sub>eq/ha\] |
| GHG.ha.N<sub>2</sub>O.Direct.CO<sub>2</sub>eq.F_ON | Direkte Lachgasemissionen aus organischer Düngung | \[kg CO<sub>2</sub>eq/ha\] |
| GHG.ha.N<sub>2</sub>O.Direct.CO<sub>2</sub>eq.F_CR | Direkte Lachgasemissionen aus Ernterückständen | \[kg CO<sub>2</sub>eq/ha\] |
| HG.ha.N<sub>2</sub>O.Vola.N<sub>2</sub>ON.F_SN | Indirekte (volatilisation) aus mineralischer Düngung Lachgasemissionen | \[kg N<sub>2</sub>O-N/ha\] |
| GHG.ha.N<sub>2</sub>O.Vola.N<sub>2</sub>ON.F_ON | Indirekte (volatilisation)aus organischer Düngung Lachgasemissionen | \[kg N<sub>2</sub>O-N/ha\] |
| GHG.ha.N<sub>2</sub>O.Vola.N<sub>2</sub>O.F_SN | Indirekte (volatilisation)aus mineralischer oongung Lachgasemissionen | \[kg N<sub>2</sub>O/ha\] |
| GHG.ha.N<sub>2</sub>O.Vola.N<sub>2</sub>O.F_ON | Indirekte (volatilisation)aus organischer Düngung Lachgasemissionen | \[kg N<sub>2</sub>O/ha\] |
| GHG.ha.N<sub>2</sub>O.Vola.CO<sub>2</sub>eq.F_SN | Indirekte (volatilisation) Lachgasemissionen aus mineralischer Düngung | \[kg CO<sub>2</sub>eq/ha\] |
| GHG.ha.N2B.Vola.CO<sub>2</sub>eq.F_ON | Indirekte (volatilisation)Lachgasemissionen aus organischer Düngung | \[kg CO<sub>2</sub>eq/ha\] |
| GHG.ha.N<sub>2</sub>O.Leach.N<sub>2</sub>ON.F_SN | Indirekte (leaching) Lachgasemissionen aus mineralischer Düngung | \[kg N<sub>2</sub>O-N/ha\] |
| GHG.ha.N<sub>2</sub>O.Leach.N<sub>2</sub>ON.F_ON | Indirekte (leaching)Lachgasemissionen aus organischer Düngung | \[kg N<sub>2</sub>O-N/ha\] |
| GHG.ha.N<sub>2</sub>O.Leach.N<sub>2</sub>ON.F_CR | Indirekte (leaching) Lachgasemissionen aus Ernterückständen | \[kg N<sub>2</sub>O-N/ha\] |
| GHG.ha.N<sub>2</sub>O.Leach.N2B.F_SN | Indirekte (leaching)Lachgasemissionen aus mineralischer oongung | \[kg N<sub>2</sub>O/ha\] |
| GHG.ha.N<sub>2</sub>O.Leach.N<sub>2</sub>O.F_ON | Indirekte (leaching) Lachgasemissionen aus organischer oongung | \[kg N<sub>2</sub>O/ha\] |
| GHG.ha.N<sub>2</sub>O.Leach.N<sub>2</sub>O.F_CR | Indirekte (leaching) Lachgasemissionen aus Ernterückständen | \[kg N<sub>2</sub>O/ha\] |
| GHG.ha.N<sub>2</sub>O.Leach.CO<sub>2</sub>eq.F_SN | Indirekte (leaching) Lachgasemissionen aus mineralischer Düngung | \[kg CO<sub>2</sub>eq/ha\] |
| GHG.ha.N<sub>2</sub>O.Leach.CO<sub>2</sub>eq.F_ON | Indirekte (leaching) Lachgasemissionen aus organischer Düngung | \[kg CO<sub>2</sub>eq/ha\] |
| GHG.ha.N<sub>2</sub>O.Leach.CO<sub>2</sub>eq.F_CR | Indirekte (leaching) Lachgasemissionen aus Ernterückständen | \[kg CO<sub>2</sub>eq/ha\] |

## Teilaggregiert, Endprodukt

| Name | Bedeutung | Einheit |
|----|----|----|
| GHG.MJ.N<sub>2</sub>O.Direct.N<sub>2</sub>ON | Direkte Lachgasemissionen | \[g N<sub>2</sub>O-N/MJ RME\] |
| GHG.MJ.N<sub>2</sub>O.Direct.N<sub>2</sub>O | Direkte Lachgasemissionen | \[g N<sub>2</sub>O/MJ RME\] |
| GHG.MJ.N<sub>2</sub>O.Direct.CO<sub>2</sub>eq | Direkte Lachgasemissionen | \[g CO<sub>2</sub>eq/MJ RME\] |
| GHG.MJ.N<sub>2</sub>O.Vola.N<sub>2</sub>ON | Indirekte (volatilisation) Lachgasemissionen | \[g N<sub>2</sub>O-N/MJ RME\] |
| GHG.MJ.N<sub>2</sub>O.Vola.N<sub>2</sub>O | Indirekte (volatilisation) Lachgasemissionen | \[g N<sub>2</sub>O/MJ RME\] |
| GHG.MJ.N<sub>2</sub>O.Vola.CO<sub>2</sub>eq | Indirekte (volatilisation) Lachgasemissionen | \[g CO<sub>2</sub>eq/MJ RME\] |
| GHG.MJ.N<sub>2</sub>O.Leach.N<sub>2</sub>ON | Indirekte (leaching) Lachgasemissionen | \[g CO<sub>2</sub>eq/MJ RME\] |
| GHG.MJ.N<sub>2</sub>O.Leach.N<sub>2</sub>O | Indirekte (leaching) Lachgasemissionen | \[g N<sub>2</sub>O/MJ RME\] |
| GHG.MJ.N<sub>2</sub>O.Leach.CO<sub>2</sub>eq | Indirekte (leaching) Lachgasemissionen | \[g CO<sub>2</sub>eq/MJ RME\] |

## Detailiert, Endprodukt

\# Ausgabe mit complete, N<sub>2</sub>O.detail.MJ.more

| Name | Bedeutung | Name |
|----|----|----|
| GHG.MJ.N<sub>2</sub>O.Direct.N<sub>2</sub>ON.F_SN | Direkte Lachgasemissionen aus mineralischer Düngung | \[g N<sub>2</sub>O-N/MJ RME\] |
| GHG.MJ.N<sub>2</sub>O.Direct.N<sub>2</sub>ON.F_ON | Direkte Lachgasemissionen aus organischer Düngung | \[g N<sub>2</sub>O-N/MJ RME\] |
| GHG.MJ.N<sub>2</sub>O.Direct.N<sub>2</sub>ON.F_CR | Direkte Lachgasemissionen aus ErnterUckstanden | \[g N<sub>2</sub>O-N/MJ RME\] |
| GHG.MJ.N<sub>2</sub>O.Direct.N<sub>2</sub>O.F_SN | Direkte Lachgasemissionen aus mineralischer Düngung | \[g N<sub>2</sub>O/MJ RME\] |
| GHG.MJ.N<sub>2</sub>O.Direct.N<sub>2</sub>O.F_ON | Direkte Lachgasemissionen aus organischer Düngung | \[g N<sub>2</sub>O/MJ RME\] |
| GHG.MJ.N<sub>2</sub>O.Direct.N<sub>2</sub>O.F_CR | Direkte Lachgasemissionen aus ErnterUckstanden | \[g N<sub>2</sub>O/MJ RME\] |
| GHG.MJ.N<sub>2</sub>O.Direct.CO<sub>2</sub>eq.F_SN | Direkte Lachgasemissionen aus mineralischer Düngung | \[g CO<sub>2</sub>eq/MJ RME\] |
| GHG.MJ.N<sub>2</sub>O.Direct.CO<sub>2</sub>eq.F_ON | Direkte Lachgasemissionen aus organischer Düngung | \[g CO<sub>2</sub>eq/MJ RME\] |
| GHG.MJ.N<sub>2</sub>O.Direct.CO<sub>2</sub>eq.F_CR | Direkte Lachgasemissionen aus ErnterUckstanden | \[g CO<sub>2</sub>eq/MJ RME\] |
| GHG.MJ.N<sub>2</sub>O.Vola.N<sub>2</sub>ON.F_SN | Indirekte (volatilisation) aus mineralischer Düngung Lachgasemissionen | \[g N<sub>2</sub>O-N/MJ RME\] |
| GHG.MJ.N<sub>2</sub>O.Vola.N<sub>2</sub>ON.F_ON | Indirekte (volatilisation) aus organischer Düngung Lachgasemissionen | \[g N<sub>2</sub>O-N/MJ RME\] |
| GHG.MJ.N<sub>2</sub>O.Vola.N<sub>2</sub>O.F_SN | Indirekte (volatilisation) aus mineralischer Düngung Lachgasemissionen | \[g N<sub>2</sub>O/MJ RME\] |
| GHG.MJ.N<sub>2</sub>O.Vola.N<sub>2</sub>O.F_ON | Indirekte (volatilisation) aus organischer Düngung Lachgasemissionen | \[g N<sub>2</sub>O/MJ RME\] |
| GHG.MJ.N<sub>2</sub>O.Vola.CO<sub>2</sub>eq.F_SN | Indirekte (volatilisation) Lachgasemissionen aus mineralischer Düngung | \[g CO<sub>2</sub>eq/MJ RME\] |
| GHG.MJ.N<sub>2</sub>O.Vola.CO<sub>2</sub>eq.F_ON | Indirekte (volatilisation) Lachgasemissionen aus organischer Düngung | \[g CO<sub>2</sub>eq/MJ RME) |
| GHG.MJ.N<sub>2</sub>O.Leach.N<sub>2</sub>ON.F_SN | Indirekte (leaching) Lachgasemissionen aus mineralischer Düngung | \[g N<sub>2</sub>O-N/MJ RME) |
| GHG.MJ.N<sub>2</sub>O.Leach.N<sub>2</sub>ON.F_ON | Indirekte (leaching) Lachgasemissionen aus organischer Düngung | \[g N<sub>2</sub>O-N/MJ RME) |
| GHG.MJ.N<sub>2</sub>O.Leach.N<sub>2</sub>ON.F_CR | Indirekte (leaching) Lachgasemissionen aus Ernteruckstanden | \[g N<sub>2</sub>O-N/MJ RME) |
| GHG.MJ.N<sub>2</sub>O.Leach.N<sub>2</sub>O.F_SN | Indirekte (leaching) Lachgasemissionen aus mineralischer Düngung | \[g N<sub>2</sub>O/MJ RME) |
| GHG.MJ.N<sub>2</sub>O.Leach.N<sub>2</sub>O.F_ON | Indirekte (leaching) Lachgasemissionen aus organischer Düngung | \[g N<sub>2</sub>O/MJ RME) |
| GHG.MJ.N<sub>2</sub>O.Leach.N<sub>2</sub>O.F_CR | Indirekte (leaching) Lachgasemissionen aus Ernteruckstanden | \[g N<sub>2</sub>O/MJ RME) |
| GHG.MJ.N<sub>2</sub>O.Leach.CO<sub>2</sub>eq.F_SN | Indirekte (leaching) Lachgasemissionen aus mineralischer Düngung | \[g CO<sub>2</sub>eq/MJ RME) |
| GHG.MJ.N<sub>2</sub>O.Leach.CO<sub>2</sub>eq.F_ON | Indirekte (leaching) Lachgasemissionen aus organischer Düngung | \[g CO<sub>2</sub>eq/MJ RME) |
| GHG.MJ.N<sub>2</sub>O.Leach.CO<sub>2</sub>eq.F_CR | Indirekte (leaching) Lachgasemissionen aus Ernteruckstanden | \[g CO<sub>2</sub>eq/MJ RME) |

\### Geschatze Biomasse/N-Aufnahme \###

\### vergleich Bilanzmethoden \###

# Geschätzte Biomasse/N-Aufnahme

## Vergleich Bilanzmethoden

\# Ausgabe mit compare

| Name | Bedeutung | Name |
|----|----|----|
| In.F_CR.DueV | N in Ernteresiduen, berechnte nach DuV | \[kg N/ha) |
| In.F_CR.INRA | N in Ernteresiduen, berechnte nach DuV | \[kg N/ha) |
| In.F_CR.IPCC | N in Ernteresiduen, berechnte nach DuV | \[kg N/ha) |
| Out.DueV.BiomassAG | Oberirdische Biomasse, berechnet nach DuV | \[kg DM/ha\] |
| Out.DueV.BiomassAGN | N in oberirdischer Biomasse, berechnet nach DuV | \[kg N/ha\] |
| Out.DueV.NHarvest | N-Abfuhr mit dem Erntegut, berechnet nach DuV | \[kg N/ha\] |
| Out.INRA.BiomassAG | Oberirdische Biomasse, berechnet mit Chalon-Database | \[kg DM/ha\] |
| Out.INRA.BiomassAGN | N in oberirdischer Biomasse, berechnet mit Chalon-Database | \[kg N/ha) |
| Out.INRA.NHarvest | N-Abfuhr mit dem Erntegut, berechnet mit Chalon-Database | \[kg N/ha\] |
| Out.INRA.RootN | N in unterirdischer Biomasse, berechnet mit Chalon-Database | \[kg N/ha\] |
| Out.IPCC.BiomassAG | Oberirdische Biomasse, berechnet nach IPCC | \[kg DM/ha) |

# Interne Zwischenergebnisse

\# Ausgabe mit internal

| Name | Bedeutung | Name |
|----|----|----|
| Int.Dry.H<sub>2</sub>O | Zu verdampfende Wassermenge | \[kg H<sub>2</sub>O/ha) |
| Int.F_CR.R_AG.IPCC | Verhältnis oberird. Ernteresiduen zu Ertrag (IPCC) | \[kg DM/kg DM\] |
| Int.F_CR.R_BG.IPCC | Verhältnis unterird. Ernteresiduen zu Ertrag (IPCC) | \[kg DM/kg DM\] |
| Int.IPCC.AG_DM | Oberirdische Ernteresiduen (IPCC) | \[Mg DM/ha\] |
| Int.IPCC.HI | Harvestindex (IPCC) | \[kg DM/kg DM\] |
