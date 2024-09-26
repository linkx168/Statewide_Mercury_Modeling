

#'  ---
<<<<<<< HEAD
#'  title: "Basic DAG Exploration"
=======
#'  title: "Distribution_of_Data"
>>>>>>> 8fa8a4b42eb164ce725327c72c2a440973f1350e
#'  author: "Mike Verhoeven"
#'  date: "26Sep2024"
#'  output: html_document
#'  editor_options: 
#'  chunk_output_type: console
#'  ---
  





install.packages('ggdag')
installed.packages('daggity')

library(ggdag)
library(dagitty)


#Make the DAG
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



