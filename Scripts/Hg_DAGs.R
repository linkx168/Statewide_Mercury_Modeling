#'  ---
#'  title: "Basic DAG Exploration"
#'  author: "Mike Verhoeven"
#'  date: "26Sep2024"
#'  output: html_document
#'  editor_options: 
#'  chunk_output_type: console
#'  ---
  



# libraries ---------------------------------------------------------------

# install.packages('ggdag')
# install.packages('daggity')


library(ggdag)
library(dagitty)
library( lavaan )


# data --------------------------------------------------------------------

#mercury data
hg <- read_csv(file.path("E:", "Shared drives", "Hansen Lab", "RESEARCH PROJECTS",
                         "Statewide mercury modeling", "Data", "Aggregated Data", "mn_lakes_hg_covariates.csv"))




lagos_cov <- read_csv(file.path("E:", "Shared drives", "Hansen Lab", "RESEARCH PROJECTS",
                         "Statewide mercury modeling", "Data", "Covariate Data", "MGLP_landuse_clarity_color_mercury_MI_MN_WI.csv"))



# p/u covariates that we need ---------------------------------------------

#modify COrson_Dosch thing to be as-expected (estimated Secchi clarity)





hg %>% 
  mutate(clarity_m_Corson_Dosch = 1.7/clarity_m_Corson_Dosch) %>% 
  {hg <<- .}
plot(data = hg, clarity_m_Corson_Dosch ~ secchi_depth_m_LAGOS_LIMNO)

lagos_cov %>% 
  select(lagoslakeid, nws_nlcd_cultcrop82_pct, nws_nlcd_past81_pct, nws_nlcd_wetwood90_pct) %>% 
  right_join( . , hg) %>% 
  {hg <<- .}


  





names(hg)



l#Make the DAG
Z_H_dag <- dagify(Hg~LakeSize, Hg~LittoralReliance, LittoralReliance~ZM, LittoralReliance~LakeSize, ZM~LakeSize, 
                  DecreasedFishing ~ Hg, DecreasedFishing ~ ZM, Clarity ~ ZM, Hg ~ Clarity, SedimentAnoxia ~ ZM, Hg ~ SedimentAnoxia, 
                  LittoralArea ~ Clarity, LittoralReliance ~ LittoralArea, Hg ~ LittoralArea, LittoralArea ~ GeometryRatio,
                  GeometryRatio ~ LakeSize, 
                  ZM ~ LakeSize, ZM ~ pH, pH ~ SO4, SO4 ~ WatershedAg, Clarity ~ WatershedAg, SO4 ~  WatershedWtlnd, 
                  Clarity ~ WatershedWtlnd, pH ~ WatershedWtlnd, Hg ~ Elevation, Hg ~ Species, Hg ~ Globalemissions
                    
                  )

#visualize
ggdag(Z_H_dag, text_col = "gray") + theme_dag_blank()

#not 100% sure what this tells us
node_dseparated(
  Z_H_dag,
  from = "ZM",
  to = "Hg",
  controlling_for = NULL,
  as_factor = TRUE
)

ggdag_drelationship(
  Z_H_dag,
  from = "ZM",
  to = "Hg",
  controlling_for = NULL,
  as_factor = TRUE
)

ggdag_drelationship(
  Z_H_dag,
  from = "ZM",
  to = "Hg",
  controlling_for = "LittoralReliance",
  as_factor = TRUE
)

ggdag_drelationship(
  Z_H_dag,
  from = "ZM",
  to = "Hg",
  controlling_for = c("LkSz"),
  as_factor = TRUE
)

ggdag_drelationship(
  Z_H_dag,
  from = "ZM",
  to = "Hg",
  controlling_for = c("LkSz", "LR"),
  as_factor = TRUE
)



adjustmentSets(
  Z_H_dag,
  exposure = "ZM",
  outcome = "Hg",
  type = c("minimal"),
  effect = c("total"),
  max.results = Inf
)


adjustmentSets(
  Z_H_dag,
  exposure = "ZM",
  outcome = "Hg",
  type = c("minimal"),
  effect = c("direct"),
  max.results = Inf
)





# Daggity Model -----------------------------------------------------------

# 
# dag {
#   bb="-7.129,-6.483,6.487,6.285"
#   Clarity [pos="0.395,2.316"]
#   DecreasedFishing [pos="2.373,-2.628"]
#   Elevation [pos="-1.359,-5.419"]
#   FishGrowthRate [pos="-3.158,1.473"]
#   GeometryRatio [pos="-5.995,0.882"]
#   Globalemissions [pos="-3.479,-4.313"]
#   Hg [outcome,pos="-0.436,-1.788"]
#   LakeSize [adjusted,pos="-3.329,-0.814"]
#   LittoralArea [pos="-2.369,3.960"]
#   LittoralReliance [pos="-1.620,0.220"]
#   SO4 [pos="5.353,3.817"]
#   SedimentAnoxia [pos="-1.148,1.652"]
#   Species [pos="1.299,-4.647"]
#   WatershedAg [pos="2.412,5.221"]
#   WatershedWtlnd [pos="3.333,3.050"]
#   ZM [exposure,pos="0.858,-0.167"]
#   pH [adjusted,pos="4.450,0.814"]
#   Clarity -> Hg
#   Clarity -> LittoralArea
#   Elevation -> Hg
#   FishGrowthRate -> Hg
#   GeometryRatio -> LittoralArea
#   Globalemissions -> Hg
#   Hg -> DecreasedFishing
#   Hg <-> LittoralArea
#   LakeSize -> GeometryRatio
#   LakeSize -> Hg
#   LakeSize -> LittoralReliance
#   LakeSize -> ZM
#   LittoralArea -> LittoralReliance
#   LittoralReliance -> Hg
#   SO4 -> pH
#   SedimentAnoxia -> Hg
#   Species -> Hg
#   WatershedAg -> Clarity
#   WatershedAg -> SO4
#   WatershedWtlnd -> Clarity
#   WatershedWtlnd -> SO4
#   WatershedWtlnd -> pH
#   ZM -> Clarity
#   ZM -> DecreasedFishing
#   ZM -> FishGrowthRate
#   ZM -> LittoralReliance
#   ZM -> SedimentAnoxia
#   pH -> ZM
# }
# 
# 

dag {
  bb="-7.129,-6.483,6.487,6.285"
  DecreasedFishing [pos="1.886,-4.493"]
  Elevation [pos="5.983,-2.939"]
  FishGrowthRate [pos="3.836,3.000"]
  GeometryRatio [pos="-1.104,-3.166"]
  Globalemissions [pos="5.808,-0.690"]
  Hg [outcome,pos="3.190,-0.609"]
  LittoralArea [pos="-1.214,-1.580"]
  LittoralReliance [pos="3.091,1.608"]
  SO4 [pos="2.467,4.197"]
  SedimentAnoxia [pos="4.756,4.537"]
  Species [pos="5.863,-1.839"]
  WatershedAg [adjusted,pos="-0.217,4.715"]
  WatershedWtlnd [adjusted,pos="1.141,4.748"]
  ZM [exposure,pos="-0.162,0.217"]
  lake_area_ha [adjusted,pos="-0.874,-4.687"]
  pH [pos="2.379,3.113"]
  secchi_depth_m_LAGOS_LIMNO [pos="-1.126,3.307"]
  Elevation -> Hg
  FishGrowthRate -> Hg
  GeometryRatio -> LittoralArea
  Globalemissions -> Hg
  Hg -> DecreasedFishing
  LittoralArea -> Hg
  LittoralArea -> LittoralReliance
  LittoralReliance -> Hg
  SO4 -> pH
  SedimentAnoxia -> Hg
  Species -> Hg
  WatershedAg -> SO4
  WatershedAg -> secchi_depth_m_LAGOS_LIMNO
  WatershedWtlnd -> SO4
  WatershedWtlnd -> pH
  WatershedWtlnd -> secchi_depth_m_LAGOS_LIMNO
  ZM -> DecreasedFishing
  ZM -> FishGrowthRate
  ZM -> LittoralReliance
  ZM -> SedimentAnoxia
  ZM -> secchi_depth_m_LAGOS_LIMNO
  lake_area_ha -> GeometryRatio
  lake_area_ha -> Hg
  lake_area_ha -> LittoralReliance
  lake_area_ha -> ZM
  pH -> ZM
  secchi_depth_m_LAGOS_LIMNO -> Hg
  secchi_depth_m_LAGOS_LIMNO -> LittoralArea
}

#V2
dag {
  bb="-7.129,-6.483,6.487,6.285"
  "SedimentAnoxia([Hg] in prey)" [pos="4.756,4.537"]
  DecreasedFishing [pos="1.886,-4.493"]
  Elevation [pos="5.983,-2.939"]
  FishGrowthRate [pos="3.836,3.000"]
  GeometryRatio [pos="-1.104,-3.166"]
  Globalemissions [pos="5.808,-0.690"]
  Hg [outcome,pos="3.190,-0.609"]
  LittoralArea [pos="-0.943,-1.291"]
  LittoralReliance [pos="3.091,1.608"]
  SO4 [pos="2.467,4.197"]
  Species [pos="5.863,-1.839"]
  WatershedAg [pos="-0.217,4.715"]
  WatershedWtlnd [pos="1.141,4.748"]
  ZM [exposure,pos="0.387,0.537"]
  calcium_availability [pos="0.862,2.833"]
  lake_area_ha [pos="-0.098,-4.640"]
  pH [pos="2.145,3.258"]
  secchi_depth_m_LAGOS_LIMNO [pos="-1.126,3.307"]
  "SedimentAnoxia([Hg] in prey)" -> Hg
  Elevation -> Hg
  FishGrowthRate -> Hg
  GeometryRatio -> LittoralArea
  Globalemissions -> Hg
  Hg -> DecreasedFishing
  LittoralArea -> LittoralReliance
  LittoralReliance -> Hg
  Species -> Hg
  WatershedAg -> SO4
  WatershedAg -> secchi_depth_m_LAGOS_LIMNO
  WatershedWtlnd -> SO4
  WatershedWtlnd -> pH
  WatershedWtlnd -> secchi_depth_m_LAGOS_LIMNO
  ZM -> "SedimentAnoxia([Hg] in prey)"
  ZM -> DecreasedFishing
  ZM -> FishGrowthRate
  ZM -> LittoralReliance
  ZM -> secchi_depth_m_LAGOS_LIMNO
  calcium_availability -> ZM
  lake_area_ha -> LittoralReliance
  lake_area_ha -> ZM
  pH -> calcium_availability
  secchi_depth_m_LAGOS_LIMNO -> "SedimentAnoxia([Hg] in prey)"
  secchi_depth_m_LAGOS_LIMNO -> LittoralArea
}




# expanded DAG with GH adds -----------------------------------------------
- This DAG combines true causal agents, and also proxies for Hg system behavior
- 


dag {
  bb="-7.129,-6.483,6.487,6.285"
  "Mercury Methylation" [pos="3.386,4.536"]
  "Mercury Transport" [pos="-3.936,-1.080"]
  "Trophic Position" [pos="5.775,1.096"]
  "Watershed Forest" [pos="-2.788,4.827"]
  DOC [pos="1.632,5.138"]
  DecreasedFishing [pos="1.886,-4.493"]
  Deposition [pos="-4.055,-3.374"]
  Elevation [pos="5.983,-2.939"]
  FishGrowthRate [pos="3.836,3.000"]
  GeometryRatio [pos="-1.104,-3.166"]
  Globalemissions [pos="5.808,-0.690"]
  Hg [outcome,pos="3.190,-0.609"]
  LittoralArea [pos="-1.214,-1.580"]
  LittoralReliance [pos="3.091,1.608"]
  SO4 [pos="0.603,1.893"]
  SedimentAnoxia [pos="5.537,4.750"]
  Species [pos="5.906,2.806"]
  WatershedAg [pos="-1.456,3.331"]
  WatershedWtlnd [pos="0.880,3.564"]
  ZM [exposure,pos="-0.162,0.217"]
  lake_area_ha [adjusted,pos="-0.874,-4.687"]
  pH [adjusted,pos="2.173,2.709"]
  secchi_depth_m_LAGOS_LIMNO [pos="-1.667,1.504"]
  "Mercury Methylation" -> Hg
  "Trophic Position" -> Hg
  "Watershed Forest" -> DOC
  DOC -> SedimentAnoxia
  DOC -> pH
  Elevation -> Hg
  FishGrowthRate -> Hg
  GeometryRatio -> LittoralArea
  Globalemissions -> Hg
  Hg -> DecreasedFishing
  LittoralArea -> Hg
  LittoralArea -> LittoralReliance
  LittoralReliance -> Hg
  SO4 -> pH
  SedimentAnoxia -> "Mercury Methylation"
  SedimentAnoxia -> Hg
  Species -> "Trophic Position"
  Species -> FishGrowthRate
  Species -> Hg
  Species -> LittoralReliance
  WatershedAg -> SO4
  WatershedAg -> secchi_depth_m_LAGOS_LIMNO
  WatershedWtlnd -> DOC
  WatershedWtlnd -> SO4
  WatershedWtlnd -> pH
  WatershedWtlnd -> secchi_depth_m_LAGOS_LIMNO
  ZM -> DecreasedFishing
  ZM -> FishGrowthRate
  ZM -> LittoralReliance
  ZM -> SedimentAnoxia
  ZM -> secchi_depth_m_LAGOS_LIMNO
  lake_area_ha -> GeometryRatio
  lake_area_ha -> Hg
  lake_area_ha -> LittoralReliance
  lake_area_ha -> ZM
  pH -> "Mercury Methylation"
  pH -> ZM
  secchi_depth_m_LAGOS_LIMNO -> "Mercury Methylation"
  secchi_depth_m_LAGOS_LIMNO -> Hg
  secchi_depth_m_LAGOS_LIMNO -> LittoralArea
}



# Think about a DAG re-work that is more mercury process specific ---------

#  - can we justify a statement that water chemistry does not affect ZMs


dag {
  "Hg+_inlake" [pos="-1.721,0.406"]
  HG_fishtissue [outcome,pos="-0.568,-1.676"]
  Hg_certain_prey [pos="-0.933,-0.918"]
  MeHG [pos="-1.157,-0.437"]
  ZM [exposure,pos="-0.541,1.641"]
  bacterial_methylation [pos="-1.224,0.223"]
  deposition [pos="-2.051,-0.411"]
  growth_rate [pos="0.061,-0.939"]
  prey_composition [pos="-0.477,-0.944"]
  sediment_anoxia [pos="-1.213,0.874"]
  water_chemistry [pos="-1.699,0.904"]
  watershed_factors [pos="-2.111,1.079"]
  "Hg+_inlake" -> MeHG
  Hg_certain_prey -> HG_fishtissue
  MeHG -> Hg_certain_prey
  ZM -> growth_rate
  ZM -> prey_composition
  ZM -> sediment_anoxia
  ZM -> water_chemistry
  bacterial_methylation -> MeHG
  deposition -> "Hg+_inlake"
  growth_rate -> HG_fishtissue
  prey_composition -> HG_fishtissue
  sediment_anoxia -> bacterial_methylation
  water_chemistry -> MeHG
  water_chemistry -> sediment_anoxia
  watershed_factors -> "Hg+_inlake"
  watershed_factors -> water_chemistry
}


# expands water chemistry into components, includes constraints on --------

dag {
  "Hg+_inlake" [pos="-1.721,0.406"]
  "lake size" [pos="0.663,1.023"]
  Conductivity [adjusted,pos="-1.949,1.693"]
  DO [pos="-1.467,1.284"]
  DOC [pos="-1.582,1.302"]
  HG_fishtissue [outcome,pos="-0.488,-1.764"]
  Hg_certain_prey [pos="-0.933,-0.918"]
  MeHG [pos="-1.189,-0.339"]
  ZM [exposure,pos="-0.541,1.641"]
  bacterial_methylation [pos="-1.220,0.012"]
  deposition [pos="-2.051,-0.411"]
  growth_rate [pos="0.061,-0.939"]
  light [pos="-0.740,0.102"]
  littoral_ratio_of_lake [pos="-0.727,-0.173"]
  pH [adjusted,pos="-1.805,1.518"]
  prey_composition [pos="-0.488,-1.090"]
  productivity [pos="0.327,0.061"]
  sediment_anoxia [pos="-1.062,0.619"]
  watershed_factors [pos="-1.757,-0.959"]
  "Hg+_inlake" -> MeHG
  "lake size" -> ZM
  Conductivity -> ZM
  Conductivity -> bacterial_methylation
  DO -> sediment_anoxia
  DOC -> light
  Hg_certain_prey -> HG_fishtissue
  MeHG -> Hg_certain_prey
  ZM -> growth_rate
  ZM -> light
  ZM -> prey_composition
  ZM -> productivity
  ZM -> sediment_anoxia
  bacterial_methylation -> MeHG
  deposition -> "Hg+_inlake"
  growth_rate -> HG_fishtissue
  light -> littoral_ratio_of_lake
  littoral_ratio_of_lake -> bacterial_methylation
  littoral_ratio_of_lake -> prey_composition
  pH -> ZM
  pH -> bacterial_methylation
  prey_composition -> HG_fishtissue
  productivity -> growth_rate
  productivity -> prey_composition
  sediment_anoxia -> bacterial_methylation
  watershed_factors -> "Hg+_inlake"
  watershed_factors -> Conductivity
  watershed_factors -> DO
  watershed_factors -> DOC
  watershed_factors -> pH
  watershed_factors -> prey_composition
  watershed_factors -> productivity
}



# denver addtions ---------------------------------------------------------

dag {
  "Hg+_inlake" [pos="-1.721,0.406"]
  "lake size" [pos="0.663,1.023"]
  HG_fishtissue [outcome,pos="-0.568,-1.676"]
  Hg_certain_prey [pos="-0.933,-0.918"]
  MeHG [pos="-1.191,-0.152"]
  ZM [exposure,pos="-0.541,1.641"]
  bacterial_methylation [pos="-1.175,0.261"]
  deposition [pos="-2.051,-0.411"]
  growth_rate [pos="0.061,-0.939"]
  littoral_ratio_of_lake [pos="-0.770,-0.322"]
  prey_composition [pos="-0.490,-0.931"]
  productivity [pos="0.327,0.061"]
  sediment_anoxia [pos="-1.213,0.874"]
  water_chemistry [pos="-1.640,0.887"]
  watershed_factors [pos="-1.917,1.483"]
  "Hg+_inlake" -> MeHG
  "lake size" -> ZM
  Hg_certain_prey -> HG_fishtissue
  MeHG -> Hg_certain_prey
  ZM -> growth_rate
  ZM -> littoral_ratio_of_lake
  ZM -> prey_composition
  ZM -> productivity
  ZM -> sediment_anoxia
  ZM -> water_chemistry
  bacterial_methylation -> MeHG
  deposition -> "Hg+_inlake"
  growth_rate -> HG_fishtissue
  littoral_ratio_of_lake -> bacterial_methylation
  littoral_ratio_of_lake -> prey_composition
  prey_composition -> HG_fishtissue
  productivity -> growth_rate
  productivity -> prey_composition
  sediment_anoxia -> bacterial_methylation
  water_chemistry -> MeHG
  water_chemistry -> bacterial_methylation
  water_chemistry -> sediment_anoxia
  watershed_factors -> "Hg+_inlake"
  watershed_factors -> productivity
  watershed_factors -> water_chemistry
}



# Moslemi Adqam subarctic NOP fish  ---------------------------------------

dag {
  "Hg+_inlake" [pos="-1.597,0.034"]
  "lake size" [pos="0.271,1.419"]
  HG_fishtissue [outcome,pos="-0.568,-1.676"]
  Hg_certain_prey [pos="-0.932,-0.995"]
  MeHG [pos="-1.191,-0.152"]
  ZM [exposure,pos="-0.541,1.641"]
  bacterial_methylation [pos="-1.175,0.261"]
  deposition [pos="-2.051,-0.411"]
  growth_rate [pos="0.061,-0.939"]
  littoral_ratio_of_lake [pos="-0.770,-0.322"]
  prey_composition [pos="-0.517,-0.964"]
  productivity [pos="0.327,0.061"]
  sediment_anoxia [pos="-1.213,0.874"]
  water_chemistry [pos="-1.640,0.887"]
  watershed_factors [pos="-1.917,1.483"]
  "Hg+_inlake" -> MeHG
  "lake size" -> ZM
  Hg_certain_prey -> HG_fishtissue
  MeHG -> Hg_certain_prey
  ZM -> growth_rate
  ZM -> littoral_ratio_of_lake
  ZM -> prey_composition
  ZM -> productivity
  ZM -> sediment_anoxia
  ZM -> water_chemistry
  bacterial_methylation -> MeHG
  deposition -> "Hg+_inlake"
  growth_rate -> HG_fishtissue
  littoral_ratio_of_lake -> bacterial_methylation
  littoral_ratio_of_lake -> prey_composition
  prey_composition -> HG_fishtissue
  productivity -> growth_rate
  productivity -> prey_composition
  sediment_anoxia -> bacterial_methylation
  water_chemistry -> MeHG
  water_chemistry -> bacterial_methylation
  water_chemistry -> growth_rate
  water_chemistry -> sediment_anoxia
  watershed_factors -> "Hg+_inlake"
  watershed_factors -> productivity
  watershed_factors -> water_chemistry
}



# Check DAG Consistency ---------------------------------------------------

#brush up data to reflect the DAG:
hg_data %>% 
  mutate(WatershedAg    = nws_nlcd_past81_pct + nws_nlcd_cultcrop82_pct,
         WatershedWtlnd = nws_nlcd_wetemerg95_pct + nws_nlcd_wetwood90_pct,
         Secchi = ifelse( is.na(secchi_depth_m_LAGOS_LIMNO), 1.7/clarity_m_Corson_Dosch , secchi_depth_m_LAGOS_LIMNO ),
         ZM = zm_sample_lag) %>% 
  {hg_data <<- .}

hg_data %>% 
  group_by(is.na(Secchi)) %>% 
  count()


hist(hg_data$Secchi)


landscape_hg_mod <- dagitty(
'dag {
bb="-7.129,-6.483,6.487,6.285"
DecreasedFishing [latent,pos="1.886,-4.493"]
Elevation [latent,pos="5.983,-2.939"]
FishGrowthRate [latent,pos="3.836,3.000"]
GeometryRatio [latent,pos="-0.840,-3.456"]
Globalemissions [latent,pos="5.808,-0.690"]
LittoralArea [latent,pos="-1.061,-2.168"]
LittoralReliance [latent,pos="3.091,1.608"]
SO4 [latent,pos="-2.686,5.437"]
Secchi [pos="-1.186,2.877"]
SedimentAnoxia [latent,pos="4.756,4.537"]
WatershedAg [adjusted,pos="1.044,4.428"]
WatershedWtlnd [adjusted,pos="-0.715,5.247"]
ZM [exposure,pos="-2.667,1.064"]
hg_ppm [outcome,pos="3.190,-0.609"]
lake_area_ha [adjusted,pos="-1.013,-4.801"]
length_in [adjusted,pos="5.497,0.493"]
pH [latent,pos="-2.754,2.892"]
spp_code [adjusted,pos="5.275,1.693"]
Elevation -> hg_ppm
FishGrowthRate -> hg_ppm
GeometryRatio -> LittoralArea
Globalemissions -> hg_ppm
LittoralArea -> LittoralReliance
LittoralArea -> hg_ppm
LittoralReliance -> hg_ppm
SO4 -> pH
Secchi -> LittoralArea
Secchi -> hg_ppm
SedimentAnoxia -> hg_ppm
WatershedAg -> SO4
WatershedAg -> Secchi
WatershedWtlnd -> SO4
WatershedWtlnd -> Secchi
WatershedWtlnd -> pH
ZM -> DecreasedFishing
ZM -> FishGrowthRate
ZM -> LittoralReliance
ZM -> Secchi
ZM -> SedimentAnoxia
hg_ppm -> DecreasedFishing
lake_area_ha -> GeometryRatio
lake_area_ha -> LittoralReliance
lake_area_ha -> ZM
lake_area_ha -> hg_ppm
length_in -> hg_ppm
pH -> ZM
spp_code -> hg_ppm
spp_code -> length_in
}
'
)

#plot the model to verify looks as expected
plot(landscape_hg_mod)



#run local tests fn to eval conditional independencies hold in the data

hg_data %>% 
  select(WatershedAg, WatershedWtlnd, Secchi, ZM, hg_ppm, lake_area_ha, spp_code, length_in) %>% 
  filter(complete.cases(.)) %>% 
  {test_dat <<- .}

corr <- lavCor( test_dat )



res <- localTests( landscape_hg_mod, sample.cov=corr, sample.nobs=nrow( test_dat ) )
print( res )


plotLocalTestResults( res )































