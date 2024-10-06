# LOAD LIBRARIES ---------------------------------------------------------------
library(terra)
library(prioriactions)
library(gurobi)
library(ggplot2)
library(tidyverse)
library(sf)

# PARAMETERS -------------------------------------------------------------------
AoI_vector <- terra::vect("Input/subregions.shp")
AoI_vector <- as.data.frame(AoI_vector)
TOTAL_RISK_Ecosystem <- terra::rast("Output/outputs/TOTAL_RISK_Ecosystem.tif")
TOTAL_RISK_Ecosystem <- as.data.frame(TOTAL_RISK_Ecosystem)
AoI_vector$TOTAL_RISK_Ecosystem <- TOTAL_RISK_Ecosystem$TOTAL_RISK_Ecosystem
boundary <- read.csv("bound.csv", sep = ";")
boundary$kNN <- 0
boundary$kNN[boundary$Distance <= 1000] <- 1

# OUTPUTS ----------------------------------------------------------------------
output_solutionsmatrix <- AoI_vector
output_sum <- data.frame(subregions = c(replicate(4, "ChileanPatagonia"),
                                        replicate(4, "LosLagos"),
                                        replicate(4, "IslaMagdalena"),
                                        replicate(4, "LasGuaitecas"),
                                        replicate(4, "Kawesqar"),
                                        replicate(4, "Other")),
                         performance_index = c(replicate(6, c("Actions", "Monitoring", "RiskReduction", "RiskReduction%"))))
# PlanningUnits <- c(nrow(AoI_vector),
#                    sum(AoI_vector$name == "LosLagos"),
#                    sum(AoI_vector$name == "IslaMagdalena"),
#                    sum(AoI_vector$name == "LasGuaitecas"),
#                    sum(AoI_vector$name == "Kawesqar"),
#                    sum(AoI_vector$name == "Other"))
ChilenPatagonia <- c(nrow(AoI_vector),
                     nrow(AoI_vector) / nrow(AoI_vector),
                     sum(AoI_vector$TOTAL_RISK_Ecosystem),
                     sum(AoI_vector$TOTAL_RISK_Ecosystem) / sum(AoI_vector$TOTAL_RISK_Ecosystem),
                     sum(AoI_vector$TOTAL_RISK_Ecosystem) / nrow(AoI_vector))
LosLagos <- c(sum(AoI_vector$name == "LosLagos"),
              sum(AoI_vector$name == "LosLagos") / nrow(AoI_vector),
              sum(AoI_vector$TOTAL_RISK_Ecosystem[AoI_vector$name == "LosLagos"]),
              sum(AoI_vector$TOTAL_RISK_Ecosystem[AoI_vector$name == "LosLagos"]) / sum(AoI_vector$TOTAL_RISK_Ecosystem),
              sum(AoI_vector$TOTAL_RISK_Ecosystem[AoI_vector$name == "LosLagos"]) / sum(AoI_vector$name == "LosLagos"))
IslaMagdalena <- c(sum(AoI_vector$name == "IslaMagdalena"),
                   sum(AoI_vector$name == "IslaMagdalena") / nrow(AoI_vector),
                   sum(AoI_vector$TOTAL_RISK_Ecosystem[AoI_vector$name == "IslaMagdalena"]),
                   sum(AoI_vector$TOTAL_RISK_Ecosystem[AoI_vector$name == "IslaMagdalena"]) / sum(AoI_vector$TOTAL_RISK_Ecosystem),
                   sum(AoI_vector$TOTAL_RISK_Ecosystem[AoI_vector$name == "IslaMagdalena"]) / sum(AoI_vector$name == "IslaMagdalena"))
LasGuaitecas <- c(sum(AoI_vector$name == "LasGuaitecas"),
                  sum(AoI_vector$name == "LasGuaitecas") / nrow(AoI_vector),
                  sum(AoI_vector$TOTAL_RISK_Ecosystem[AoI_vector$name == "LasGuaitecas"]),
                  sum(AoI_vector$TOTAL_RISK_Ecosystem[AoI_vector$name == "LasGuaitecas"]) / sum(AoI_vector$TOTAL_RISK_Ecosystem),
                  sum(AoI_vector$TOTAL_RISK_Ecosystem[AoI_vector$name == "LasGuaitecas"]) / sum(AoI_vector$name == "LasGuaitecas"))
Kawesqar <- c(sum(AoI_vector$name == "Kawesqar"),
              sum(AoI_vector$name == "Kawesqar") / nrow(AoI_vector),
              sum(AoI_vector$TOTAL_RISK_Ecosystem[AoI_vector$name == "Kawesqar"]),
              sum(AoI_vector$TOTAL_RISK_Ecosystem[AoI_vector$name == "Kawesqar"]) / sum(AoI_vector$TOTAL_RISK_Ecosystem),
              sum(AoI_vector$TOTAL_RISK_Ecosystem[AoI_vector$name == "Kawesqar"]) / sum(AoI_vector$name == "Kawesqar"))
Other <- c(sum(AoI_vector$name == "Other"),
           sum(AoI_vector$name == "Other") / nrow(AoI_vector),
           sum(AoI_vector$TOTAL_RISK_Ecosystem[AoI_vector$name == "Other"]),
           sum(AoI_vector$TOTAL_RISK_Ecosystem[AoI_vector$name == "Other"]) / sum(AoI_vector$TOTAL_RISK_Ecosystem),
           sum(AoI_vector$TOTAL_RISK_Ecosystem[AoI_vector$name == "Other"]) / sum(AoI_vector$name == "Other"))
output_stats <- data.frame(subregions = c("PlanningUnits", "PlanningUnits%", "CumulativeRisk", "CumulativeRisk%", "AvgRisk"),
                           ChileanPatagonia = ChilenPatagonia,
                           LosLagos = LosLagos,
                           IslaMagdalena = IslaMagdalena,
                           LasGuaitecas = LasGuaitecas,
                           Kawesqar = Kawesqar,
                           Other = Other)
write.csv(output_stats, file = "output_stats.csv", row.names = FALSE)

for (r in c(0.1, 0.2)){
  
  ##############################################################################
  # RISK MANAGEMENT APPROACH
  ##############################################################################
  
  # NO CONNECTED SCENARIO ------------------------------------------------------
  R <- sum(AoI_vector$TOTAL_RISK_Ecosystem, na.rm = TRUE)
  
  # BUILD PU DATAFRAME
  pu_data <- data.frame(id = AoI_vector$id,
                        monitoring_cost = 1,
                        status = 0)
  
  # BUILD FEATURE DATAFRAME
  features_data <- data.frame(id = 1,
                              target_recovery = r * R,
                              name = "Kelp_Forest")
  
  # BUILD FEATURE DISTRIBUTION DATAFRAME
  dist_features_data <- data.frame(pu = AoI_vector$id,
                                   amount = AoI_vector$TOTAL_RISK_Ecosystem,
                                   feature = 1)
  dist_features_data <- dist_features_data[dist_features_data$amount > 0,]
  
  # BUILD THREAT DATAFRAME
  threats_data <- data.frame(id = 1,
                             blm_action = 0,
                             name = "FROM ALL STRESSORS")
  
  # BUILD THREAT DISTRIBUTION DATAFRAME
  dist_threats_data <- data.frame(pu = dist_features_data$pu,
                                  threat = 1,
                                  amount = 1,
                                  action_cost = 1,
                                  status = 0)
  
  # BUILD SENSITIVITY DATAFRAME
  sensitivity_data <- data.frame(feature = 1,
                                 threat = 1)
  
  # CREATE PROBLEM
  d <- inputData(pu = pu_data,
                 features = features_data,
                 dist_features = dist_features_data,
                 threats = threats_data,
                 dist_threats = dist_threats_data,
                 sensitivity = sensitivity_data)
  
  # CREATE MODEL
  p <- problem(d, model_type = "minimizeCosts", blm = 0)
  
  # SOLVE MODEL
  s <- solve(p, solver = "gurobi", cores = 8, verbose = TRUE, output_file = FALSE)
  
  # SAVE SOLUTIONS
  output_solutionsmatrix$no_connect <- replace(getActions(s)$"1", getActions(s)$"1" == 1, 2) + getActions(s)$connectivity
  
  # SAVE PERFORMANCE INDICATORS
  no_connect_perf_idx <- c()
  
  # Chilean Patagonia
  no_connect_perf_idx <- c(no_connect_perf_idx, sum(getActions(s)$"1"))
  no_connect_perf_idx <- c(no_connect_perf_idx, sum(getActions(s)$connectivity))
  no_connect_perf_idx <- c(no_connect_perf_idx, getSolutionBenefit(s)$benefit.total)
  no_connect_perf_idx <- c(no_connect_perf_idx, 100 * (getSolutionBenefit(s)$benefit.total / (r * R)))
  
  # Los Lagos
  no_connect_perf_idx <- c(no_connect_perf_idx, sum(output_solutionsmatrix$no_connect[output_solutionsmatrix$name == "LosLagos" & output_solutionsmatrix$no_connect == 2]) / 2)
  no_connect_perf_idx <- c(no_connect_perf_idx, sum(output_solutionsmatrix$no_connect[output_solutionsmatrix$name == "LosLagos" & output_solutionsmatrix$no_connect == 1]))
  no_connect_perf_idx <- c(no_connect_perf_idx, sum(output_solutionsmatrix$TOTAL_RISK_Ecosystem[output_solutionsmatrix$name == "LosLagos" & output_solutionsmatrix$no_connect == 2]))
  no_connect_perf_idx <- c(no_connect_perf_idx, 100 * sum(output_solutionsmatrix$TOTAL_RISK_Ecosystem[output_solutionsmatrix$name == "LosLagos" & output_solutionsmatrix$no_connect == 2]) / (r * R))
  
  # Isla Magdalena
  no_connect_perf_idx <- c(no_connect_perf_idx, sum(output_solutionsmatrix$no_connect[output_solutionsmatrix$name == "IslaMagdalena" & output_solutionsmatrix$no_connect == 2]) / 2)
  no_connect_perf_idx <- c(no_connect_perf_idx, sum(output_solutionsmatrix$no_connect[output_solutionsmatrix$name == "IslaMagdalena" & output_solutionsmatrix$no_connect == 1]))
  no_connect_perf_idx <- c(no_connect_perf_idx, sum(output_solutionsmatrix$TOTAL_RISK_Ecosystem[output_solutionsmatrix$name == "IslaMagdalena" & output_solutionsmatrix$no_connect == 2]))
  no_connect_perf_idx <- c(no_connect_perf_idx, 100 * sum(output_solutionsmatrix$TOTAL_RISK_Ecosystem[output_solutionsmatrix$name == "IslaMagdalena" & output_solutionsmatrix$no_connect == 2]) / (r * R))
  
  # Las Guaitecas
  no_connect_perf_idx <- c(no_connect_perf_idx, sum(output_solutionsmatrix$no_connect[output_solutionsmatrix$name == "LasGuaitecas" & output_solutionsmatrix$no_connect == 2]) / 2)
  no_connect_perf_idx <- c(no_connect_perf_idx, sum(output_solutionsmatrix$no_connect[output_solutionsmatrix$name == "LasGuaitecas" & output_solutionsmatrix$no_connect == 1]))
  no_connect_perf_idx <- c(no_connect_perf_idx, sum(output_solutionsmatrix$TOTAL_RISK_Ecosystem[output_solutionsmatrix$name == "LasGuaitecas" & output_solutionsmatrix$no_connect == 2]))
  no_connect_perf_idx <- c(no_connect_perf_idx, 100 * sum(output_solutionsmatrix$TOTAL_RISK_Ecosystem[output_solutionsmatrix$name == "LasGuaitecas" & output_solutionsmatrix$no_connect == 2]) / (r * R))
  
  # Kawésqar
  no_connect_perf_idx <- c(no_connect_perf_idx, sum(output_solutionsmatrix$no_connect[output_solutionsmatrix$name == "Kawesqar" & output_solutionsmatrix$no_connect == 2]) / 2)
  no_connect_perf_idx <- c(no_connect_perf_idx, sum(output_solutionsmatrix$no_connect[output_solutionsmatrix$name == "Kawesqar" & output_solutionsmatrix$no_connect == 1]))
  no_connect_perf_idx <- c(no_connect_perf_idx, sum(output_solutionsmatrix$TOTAL_RISK_Ecosystem[output_solutionsmatrix$name == "Kawesqar" & output_solutionsmatrix$no_connect == 2]))
  no_connect_perf_idx <- c(no_connect_perf_idx, 100 * sum(output_solutionsmatrix$TOTAL_RISK_Ecosystem[output_solutionsmatrix$name == "Kawesqar" & output_solutionsmatrix$no_connect == 2]) / (r * R))
  
  # Other
  no_connect_perf_idx <- c(no_connect_perf_idx, sum(output_solutionsmatrix$no_connect[output_solutionsmatrix$name == "Other" & output_solutionsmatrix$no_connect == 2]) / 2)
  no_connect_perf_idx <- c(no_connect_perf_idx, sum(output_solutionsmatrix$no_connect[output_solutionsmatrix$name == "Other" & output_solutionsmatrix$no_connect == 1]))
  no_connect_perf_idx <- c(no_connect_perf_idx, sum(output_solutionsmatrix$TOTAL_RISK_Ecosystem[output_solutionsmatrix$name == "Other" & output_solutionsmatrix$no_connect == 2]))
  no_connect_perf_idx <- c(no_connect_perf_idx, 100 * sum(output_solutionsmatrix$TOTAL_RISK_Ecosystem[output_solutionsmatrix$name == "Other" & output_solutionsmatrix$no_connect == 2]) / (r * R))
  
  output_sum$no_connect <- no_connect_perf_idx
  colnames(output_sum)[(colnames(output_sum) == "no_connect")] <- paste0("no_connect_", r)
  colnames(output_solutionsmatrix)[(colnames(output_solutionsmatrix) == "no_connect")] <- paste0("no_connect_", r)
  
  # CONNECTED SCENARIO ---------------------------------------------------------
  R <- sum(AoI_vector$TOTAL_RISK_Ecosystem, na.rm = TRUE)
  
  # BUILD PU DATAFRAME
  pu_data <- data.frame(id = AoI_vector$id,
                        monitoring_cost = 1,
                        status = 0)
  
  # BUILD FEATURE DATAFRAME
  features_data <- data.frame(id = 1,
                              target_recovery = r * R,
                              name = "Kelp_Forest")
  
  # BUILD FEATURE DISTRIBUTION DATAFRAME
  dist_features_data <- data.frame(pu = AoI_vector$id,
                                   amount = AoI_vector$TOTAL_RISK_Ecosystem,
                                   feature = 1)
  dist_features_data <- dist_features_data[dist_features_data$amount > 0,]
  
  # BUILD THREAT DATAFRAME
  threats_data <- data.frame(id = 1,
                             blm_action = 0,
                             name = "FROM ALL STRESSORS")
  
  # BUILD THREAT DISTRIBUTION DATAFRAME
  dist_threats_data <- data.frame(pu = dist_features_data$pu,
                                  threat = 1,
                                  amount = 1,
                                  action_cost = 1,
                                  status = 0)
  
  # BUILD SENSITIVITY DATAFRAME
  sensitivity_data <- data.frame(feature = 1,
                                 threat = 1)
  
  # BUILD BOUNDARY DATAFRAME
  boundary_data <- data.frame(id1 = boundary$InputID,
                              id2 = boundary$TargetID,
                              boundary = boundary$kNN)
  boundary_data <- boundary_data[boundary_data$boundary > 0,]
  
  # CREATE PROBLEM
  d <- inputData(pu = pu_data,
                 features = features_data,
                 dist_features = dist_features_data,
                 threats = threats_data,
                 dist_threats = dist_threats_data,
                 sensitivity = sensitivity_data,
                 boundary = boundary_data)
  
  # CREATE MODEL
  p <- problem(d, model_type = "minimizeCosts", blm = 20)
  
  # SOLVE MODEL
  s <- solve(p, solver = "gurobi", cores = 8, verbose = TRUE, output_file = FALSE)
  
  # SAVE SOLUTIONS
  output_solutionsmatrix$connect <- replace(getActions(s)$"1", getActions(s)$"1" == 1, 2) + getActions(s)$connectivity
  
  # SAVE PERFORMANCE INDICATORS
  connect_perf_idx <- c()
  
  # Chilean Patagonia
  connect_perf_idx <- c(connect_perf_idx, sum(getActions(s)$"1"))
  connect_perf_idx <- c(connect_perf_idx, sum(getActions(s)$connectivity))
  connect_perf_idx <- c(connect_perf_idx, getSolutionBenefit(s)$benefit.total)
  connect_perf_idx <- c(connect_perf_idx, 100 * (getSolutionBenefit(s)$benefit.total / (r * R)))
  
  # Los Lagos
  connect_perf_idx <- c(connect_perf_idx, sum(output_solutionsmatrix$connect[output_solutionsmatrix$name == "LosLagos" & output_solutionsmatrix$connect == 2]) / 2)
  connect_perf_idx <- c(connect_perf_idx, sum(output_solutionsmatrix$connect[output_solutionsmatrix$name == "LosLagos" & output_solutionsmatrix$connect == 1]))
  connect_perf_idx <- c(connect_perf_idx, sum(output_solutionsmatrix$TOTAL_RISK_Ecosystem[output_solutionsmatrix$name == "LosLagos" & output_solutionsmatrix$connect == 2]))
  connect_perf_idx <- c(connect_perf_idx, 100 * sum(output_solutionsmatrix$TOTAL_RISK_Ecosystem[output_solutionsmatrix$name == "LosLagos" & output_solutionsmatrix$connect == 2]) / (r * R))
  
  # Isla Magdalena
  connect_perf_idx <- c(connect_perf_idx, sum(output_solutionsmatrix$connect[output_solutionsmatrix$name == "IslaMagdalena" & output_solutionsmatrix$connect == 2]) / 2)
  connect_perf_idx <- c(connect_perf_idx, sum(output_solutionsmatrix$connect[output_solutionsmatrix$name == "IslaMagdalena" & output_solutionsmatrix$connect == 1]))
  connect_perf_idx <- c(connect_perf_idx, sum(output_solutionsmatrix$TOTAL_RISK_Ecosystem[output_solutionsmatrix$name == "IslaMagdalena" & output_solutionsmatrix$connect == 2]))
  connect_perf_idx <- c(connect_perf_idx, 100 * sum(output_solutionsmatrix$TOTAL_RISK_Ecosystem[output_solutionsmatrix$name == "IslaMagdalena" & output_solutionsmatrix$connect == 2]) / (r * R))
  
  # Las Guaitecas
  connect_perf_idx <- c(connect_perf_idx, sum(output_solutionsmatrix$connect[output_solutionsmatrix$name == "LasGuaitecas" & output_solutionsmatrix$connect == 2]) / 2)
  connect_perf_idx <- c(connect_perf_idx, sum(output_solutionsmatrix$connect[output_solutionsmatrix$name == "LasGuaitecas" & output_solutionsmatrix$connect == 1]))
  connect_perf_idx <- c(connect_perf_idx, sum(output_solutionsmatrix$TOTAL_RISK_Ecosystem[output_solutionsmatrix$name == "LasGuaitecas" & output_solutionsmatrix$connect == 2]))
  connect_perf_idx <- c(connect_perf_idx, 100 * sum(output_solutionsmatrix$TOTAL_RISK_Ecosystem[output_solutionsmatrix$name == "LasGuaitecas" & output_solutionsmatrix$connect == 2]) / (r * R))
  
  # Kawésqar
  connect_perf_idx <- c(connect_perf_idx, sum(output_solutionsmatrix$connect[output_solutionsmatrix$name == "Kawesqar" & output_solutionsmatrix$connect == 2]) / 2)
  connect_perf_idx <- c(connect_perf_idx, sum(output_solutionsmatrix$connect[output_solutionsmatrix$name == "Kawesqar" & output_solutionsmatrix$connect == 1]))
  connect_perf_idx <- c(connect_perf_idx, sum(output_solutionsmatrix$TOTAL_RISK_Ecosystem[output_solutionsmatrix$name == "Kawesqar" & output_solutionsmatrix$connect == 2]))
  connect_perf_idx <- c(connect_perf_idx, 100 * sum(output_solutionsmatrix$TOTAL_RISK_Ecosystem[output_solutionsmatrix$name == "Kawesqar" & output_solutionsmatrix$connect == 2]) / (r * R))
  
  # Other
  connect_perf_idx <- c(connect_perf_idx, sum(output_solutionsmatrix$connect[output_solutionsmatrix$name == "Other" & output_solutionsmatrix$connect == 2]) / 2)
  connect_perf_idx <- c(connect_perf_idx, sum(output_solutionsmatrix$connect[output_solutionsmatrix$name == "Other" & output_solutionsmatrix$connect == 1]))
  connect_perf_idx <- c(connect_perf_idx, sum(output_solutionsmatrix$TOTAL_RISK_Ecosystem[output_solutionsmatrix$name == "Other" & output_solutionsmatrix$connect == 2]))
  connect_perf_idx <- c(connect_perf_idx, 100 * sum(output_solutionsmatrix$TOTAL_RISK_Ecosystem[output_solutionsmatrix$name == "Other" & output_solutionsmatrix$connect == 2]) / (r * R))
  
  output_sum$connect <- connect_perf_idx
  colnames(output_sum)[(colnames(output_sum) == "connect")] <- paste0("connect_", r)
  colnames(output_solutionsmatrix)[(colnames(output_solutionsmatrix) == "connect")] <- paste0("connect_", r)
  
  # LOCKED-OUT SCENARIO --------------------------------------------------------
  R <- sum(AoI_vector$TOTAL_RISK_Ecosystem, na.rm = TRUE)
  
  # BUILD PU DATAFRAME
  pu_data <- data.frame(id = AoI_vector$id,
                        monitoring_cost = 1,
                        status = AoI_vector$status)
  
  # BUILD FEATURE DATAFRAME
  features_data <- data.frame(id = 1,
                              target_recovery = r * R,
                              name = "Kelp_Forest")
  
  # BUILD FEATURE DISTRIBUTION DATAFRAME
  dist_features_data <- data.frame(pu = AoI_vector$id,
                                   amount = AoI_vector$TOTAL_RISK_Ecosystem,
                                   feature = 1)
  dist_features_data <- dist_features_data[dist_features_data$amount > 0,]
  
  # BUILD THREAT DATAFRAME
  threats_data <- data.frame(id = 1,
                             blm_action = 0,
                             name = "FROM ALL STRESSORS")
  
  # BUILD THREAT DISTRIBUTION DATAFRAME
  dist_threats_data <- data.frame(pu = dist_features_data$pu,
                                  threat = 1,
                                  amount = 1,
                                  action_cost = 1,
                                  status = 0)
  
  # BUILD SENSITIVITY DATAFRAME
  sensitivity_data <- data.frame(feature = 1,
                                 threat = 1)
  
  # BUILD BOUNDARY DATAFRAME
  boundary_data <- data.frame(id1 = boundary$InputID,
                              id2 = boundary$TargetID,
                              boundary = boundary$kNN)
  boundary_data <- boundary_data[boundary_data$boundary > 0,]
  
  # CREATE PROBLEM
  d <- inputData(pu = pu_data,
                 features = features_data,
                 dist_features = dist_features_data,
                 threats = threats_data,
                 dist_threats = dist_threats_data,
                 sensitivity = sensitivity_data,
                 boundary = boundary_data)
  
  # CREATE MODEL
  p <- problem(d, model_type = "minimizeCosts", blm = 20)
  
  # SOLVE MODEL
  s <- solve(p, solver = "gurobi", cores = 8, verbose = TRUE, output_file = FALSE)
  
  # SAVE SOLUTIONS
  output_solutionsmatrix$locke_out <- replace(getActions(s)$"1", getActions(s)$"1" == 1, 2) + getActions(s)$connectivity
  
  # SAVE PERFORMANCE INDICATORS
  locke_out_perf_idx <- c()
  
  # Chilean Patagonia
  locke_out_perf_idx <- c(locke_out_perf_idx, sum(getActions(s)$"1"))
  locke_out_perf_idx <- c(locke_out_perf_idx, sum(getActions(s)$connectivity))
  locke_out_perf_idx <- c(locke_out_perf_idx, getSolutionBenefit(s)$benefit.total)
  locke_out_perf_idx <- c(locke_out_perf_idx, 100 * (getSolutionBenefit(s)$benefit.total / (r * R)))
  
  # Los Lagos
  locke_out_perf_idx <- c(locke_out_perf_idx, sum(output_solutionsmatrix$locke_out[output_solutionsmatrix$name == "LosLagos" & output_solutionsmatrix$locke_out == 2]) / 2)
  locke_out_perf_idx <- c(locke_out_perf_idx, sum(output_solutionsmatrix$locke_out[output_solutionsmatrix$name == "LosLagos" & output_solutionsmatrix$locke_out == 1]))
  locke_out_perf_idx <- c(locke_out_perf_idx, sum(output_solutionsmatrix$TOTAL_RISK_Ecosystem[output_solutionsmatrix$name == "LosLagos" & output_solutionsmatrix$locke_out == 2]))
  locke_out_perf_idx <- c(locke_out_perf_idx, 100 * sum(output_solutionsmatrix$TOTAL_RISK_Ecosystem[output_solutionsmatrix$name == "LosLagos" & output_solutionsmatrix$locke_out == 2]) / (r * R))
  
  # Isla Magdalena
  locke_out_perf_idx <- c(locke_out_perf_idx, sum(output_solutionsmatrix$locke_out[output_solutionsmatrix$name == "IslaMagdalena" & output_solutionsmatrix$locke_out == 2]) / 2)
  locke_out_perf_idx <- c(locke_out_perf_idx, sum(output_solutionsmatrix$locke_out[output_solutionsmatrix$name == "IslaMagdalena" & output_solutionsmatrix$locke_out == 1]))
  locke_out_perf_idx <- c(locke_out_perf_idx, sum(output_solutionsmatrix$TOTAL_RISK_Ecosystem[output_solutionsmatrix$name == "IslaMagdalena" & output_solutionsmatrix$locke_out == 2]))
  locke_out_perf_idx <- c(locke_out_perf_idx, 100 * sum(output_solutionsmatrix$TOTAL_RISK_Ecosystem[output_solutionsmatrix$name == "IslaMagdalena" & output_solutionsmatrix$locke_out == 2]) / (r * R))
  
  # Las Guaitecas
  locke_out_perf_idx <- c(locke_out_perf_idx, sum(output_solutionsmatrix$locke_out[output_solutionsmatrix$name == "LasGuaitecas" & output_solutionsmatrix$locke_out == 2]) / 2)
  locke_out_perf_idx <- c(locke_out_perf_idx, sum(output_solutionsmatrix$locke_out[output_solutionsmatrix$name == "LasGuaitecas" & output_solutionsmatrix$locke_out == 1]))
  locke_out_perf_idx <- c(locke_out_perf_idx, sum(output_solutionsmatrix$TOTAL_RISK_Ecosystem[output_solutionsmatrix$name == "LasGuaitecas" & output_solutionsmatrix$locke_out == 2]))
  locke_out_perf_idx <- c(locke_out_perf_idx, 100 * sum(output_solutionsmatrix$TOTAL_RISK_Ecosystem[output_solutionsmatrix$name == "LasGuaitecas" & output_solutionsmatrix$locke_out == 2]) / (r * R))
  
  # Kawésqar
  locke_out_perf_idx <- c(locke_out_perf_idx, sum(output_solutionsmatrix$locke_out[output_solutionsmatrix$name == "Kawesqar" & output_solutionsmatrix$locke_out == 2]) / 2)
  locke_out_perf_idx <- c(locke_out_perf_idx, sum(output_solutionsmatrix$locke_out[output_solutionsmatrix$name == "Kawesqar" & output_solutionsmatrix$locke_out == 1]))
  locke_out_perf_idx <- c(locke_out_perf_idx, sum(output_solutionsmatrix$TOTAL_RISK_Ecosystem[output_solutionsmatrix$name == "Kawesqar" & output_solutionsmatrix$locke_out == 2]))
  locke_out_perf_idx <- c(locke_out_perf_idx, 100 * sum(output_solutionsmatrix$TOTAL_RISK_Ecosystem[output_solutionsmatrix$name == "Kawesqar" & output_solutionsmatrix$locke_out == 2]) / (r * R))
  
  # Other
  locke_out_perf_idx <- c(locke_out_perf_idx, sum(output_solutionsmatrix$locke_out[output_solutionsmatrix$name == "Other" & output_solutionsmatrix$locke_out == 2]) / 2)
  locke_out_perf_idx <- c(locke_out_perf_idx, sum(output_solutionsmatrix$locke_out[output_solutionsmatrix$name == "Other" & output_solutionsmatrix$locke_out == 1]))
  locke_out_perf_idx <- c(locke_out_perf_idx, sum(output_solutionsmatrix$TOTAL_RISK_Ecosystem[output_solutionsmatrix$name == "Other" & output_solutionsmatrix$locke_out == 2]))
  locke_out_perf_idx <- c(locke_out_perf_idx, 100 * sum(output_solutionsmatrix$TOTAL_RISK_Ecosystem[output_solutionsmatrix$name == "Other" & output_solutionsmatrix$locke_out == 2]) / (r * R))
  
  output_sum$locke_out <- locke_out_perf_idx
  colnames(output_sum)[(colnames(output_sum) == "locke_out")] <- paste0("locke_out_", r)
  colnames(output_solutionsmatrix)[(colnames(output_solutionsmatrix) == "locke_out")] <- paste0("locke_out_", r)
  
  # AGGREGATED SCENARIO --------------------------------------------------------
  R <- sum(AoI_vector$TOTAL_RISK_Ecosystem[AoI_vector$name == "IslaMagdalena" | AoI_vector$name == "LasGuaitecas" | AoI_vector$name == "Kawesqar"], na.rm = TRUE)
  
  # BUILD PU DATAFRAME
  pu_data <- data.frame(id = AoI_vector$id,
                        monitoring_cost = 1,
                        status = AoI_vector$status)
  
  # BUILD FEATURE DATAFRAME
  features_data <- data.frame(id = 1,
                              target_recovery = r * R,
                              name = "Kelp_Forest")
  
  # BUILD FEATURE DISTRIBUTION DATAFRAME
  dist_features_data <- data.frame(pu = AoI_vector$id[AoI_vector$name == "IslaMagdalena" | AoI_vector$name == "LasGuaitecas" | AoI_vector$name == "Kawesqar"],
                                   amount = AoI_vector$TOTAL_RISK_Ecosystem[AoI_vector$name == "IslaMagdalena" | AoI_vector$name == "LasGuaitecas" | AoI_vector$name == "Kawesqar"],
                                   feature = 1)
  dist_features_data <- dist_features_data[dist_features_data$amount > 0,]
  
  # BUILD THREAT DATAFRAME
  threats_data <- data.frame(id = 1,
                             blm_action = 0,
                             name = "FROM ALL STRESSORS")
  
  # BUILD THREAT DISTRIBUTION DATAFRAME
  dist_threats_data <- data.frame(pu = dist_features_data$pu,
                                  threat = 1,
                                  amount = 1,
                                  action_cost = 1,
                                  status = 0)
  
  # BUILD SENSITIVITY DATAFRAME
  sensitivity_data <- data.frame(feature = 1,
                                 threat = 1)
  
  # BUILD BOUNDARY DATAFRAME
  boundary_data <- data.frame(id1 = boundary$InputID,
                              id2 = boundary$TargetID,
                              boundary = boundary$kNN)
  boundary_data <- boundary_data[boundary_data$boundary > 0,]
  
  # CREATE PROBLEM
  d <- inputData(pu = pu_data,
                 features = features_data,
                 dist_features = dist_features_data,
                 threats = threats_data,
                 dist_threats = dist_threats_data,
                 sensitivity = sensitivity_data,
                 boundary = boundary_data)
  
  # CREATE MODEL
  p <- problem(d, model_type = "minimizeCosts", blm = 20)
  
  # SOLVE MODEL
  s <- solve(p, solver = "gurobi", cores = 8, verbose = TRUE, output_file = FALSE)
  
  # SAVE SOLUTIONS
  output_solutionsmatrix$aggregate <- replace(getActions(s)$"1", getActions(s)$"1" == 1, 2) + getActions(s)$connectivity
  
  # SAVE PERFORMANCE INDICATORS
  aggregate_perf_idx <- c()
  
  # Chilean Patagonia
  aggregate_perf_idx <- c(aggregate_perf_idx, sum(getActions(s)$"1"))
  aggregate_perf_idx <- c(aggregate_perf_idx, sum(getActions(s)$connectivity))
  aggregate_perf_idx <- c(aggregate_perf_idx, getSolutionBenefit(s)$benefit.total)
  aggregate_perf_idx <- c(aggregate_perf_idx, 100 * (getSolutionBenefit(s)$benefit.total / (r * R)))
  
  # Los Lagos
  aggregate_perf_idx <- c(aggregate_perf_idx, sum(output_solutionsmatrix$aggregate[output_solutionsmatrix$name == "LosLagos" & output_solutionsmatrix$aggregate == 2]) / 2)
  aggregate_perf_idx <- c(aggregate_perf_idx, sum(output_solutionsmatrix$aggregate[output_solutionsmatrix$name == "LosLagos" & output_solutionsmatrix$aggregate == 1]))
  aggregate_perf_idx <- c(aggregate_perf_idx, sum(output_solutionsmatrix$TOTAL_RISK_Ecosystem[output_solutionsmatrix$name == "LosLagos" & output_solutionsmatrix$aggregate == 2]))
  aggregate_perf_idx <- c(aggregate_perf_idx, 100 * sum(output_solutionsmatrix$TOTAL_RISK_Ecosystem[output_solutionsmatrix$name == "LosLagos" & output_solutionsmatrix$aggregate == 2]) / (r * R))
  
  # Isla Magdalena
  aggregate_perf_idx <- c(aggregate_perf_idx, sum(output_solutionsmatrix$aggregate[output_solutionsmatrix$name == "IslaMagdalena" & output_solutionsmatrix$aggregate == 2]) / 2)
  aggregate_perf_idx <- c(aggregate_perf_idx, sum(output_solutionsmatrix$aggregate[output_solutionsmatrix$name == "IslaMagdalena" & output_solutionsmatrix$aggregate == 1]))
  aggregate_perf_idx <- c(aggregate_perf_idx, sum(output_solutionsmatrix$TOTAL_RISK_Ecosystem[output_solutionsmatrix$name == "IslaMagdalena" & output_solutionsmatrix$aggregate == 2]))
  aggregate_perf_idx <- c(aggregate_perf_idx, 100 * sum(output_solutionsmatrix$TOTAL_RISK_Ecosystem[output_solutionsmatrix$name == "IslaMagdalena" & output_solutionsmatrix$aggregate == 2]) / (r * R))
  
  # Las Guaitecas
  aggregate_perf_idx <- c(aggregate_perf_idx, sum(output_solutionsmatrix$aggregate[output_solutionsmatrix$name == "LasGuaitecas" & output_solutionsmatrix$aggregate == 2]) / 2)
  aggregate_perf_idx <- c(aggregate_perf_idx, sum(output_solutionsmatrix$aggregate[output_solutionsmatrix$name == "LasGuaitecas" & output_solutionsmatrix$aggregate == 1]))
  aggregate_perf_idx <- c(aggregate_perf_idx, sum(output_solutionsmatrix$TOTAL_RISK_Ecosystem[output_solutionsmatrix$name == "LasGuaitecas" & output_solutionsmatrix$aggregate == 2]))
  aggregate_perf_idx <- c(aggregate_perf_idx, 100 * sum(output_solutionsmatrix$TOTAL_RISK_Ecosystem[output_solutionsmatrix$name == "LasGuaitecas" & output_solutionsmatrix$aggregate == 2]) / (r * R))
  
  # Kawésqar
  aggregate_perf_idx <- c(aggregate_perf_idx, sum(output_solutionsmatrix$aggregate[output_solutionsmatrix$name == "Kawesqar" & output_solutionsmatrix$aggregate == 2]) / 2)
  aggregate_perf_idx <- c(aggregate_perf_idx, sum(output_solutionsmatrix$aggregate[output_solutionsmatrix$name == "Kawesqar" & output_solutionsmatrix$aggregate == 1]))
  aggregate_perf_idx <- c(aggregate_perf_idx, sum(output_solutionsmatrix$TOTAL_RISK_Ecosystem[output_solutionsmatrix$name == "Kawesqar" & output_solutionsmatrix$aggregate == 2]))
  aggregate_perf_idx <- c(aggregate_perf_idx, 100 * sum(output_solutionsmatrix$TOTAL_RISK_Ecosystem[output_solutionsmatrix$name == "Kawesqar" & output_solutionsmatrix$aggregate == 2]) / (r * R))
  
  # Other
  aggregate_perf_idx <- c(aggregate_perf_idx, sum(output_solutionsmatrix$aggregate[output_solutionsmatrix$name == "Other" & output_solutionsmatrix$aggregate == 2]) / 2)
  aggregate_perf_idx <- c(aggregate_perf_idx, sum(output_solutionsmatrix$aggregate[output_solutionsmatrix$name == "Other" & output_solutionsmatrix$aggregate == 1]))
  aggregate_perf_idx <- c(aggregate_perf_idx, sum(output_solutionsmatrix$TOTAL_RISK_Ecosystem[output_solutionsmatrix$name == "Other" & output_solutionsmatrix$aggregate == 2]))
  aggregate_perf_idx <- c(aggregate_perf_idx, 100 * sum(output_solutionsmatrix$TOTAL_RISK_Ecosystem[output_solutionsmatrix$name == "Other" & output_solutionsmatrix$aggregate == 2]) / (r * R))
  
  output_sum$aggregate <- aggregate_perf_idx
  colnames(output_sum)[(colnames(output_sum) == "aggregate")] <- paste0("aggregate_", r)
  colnames(output_solutionsmatrix)[(colnames(output_solutionsmatrix) == "aggregate")] <- paste0("aggregate_", r)
  
  # DISAGGREGATED SCENARIO -----------------------------------------------------
  R <- sum(AoI_vector$TOTAL_RISK_Ecosystem[AoI_vector$name == "IslaMagdalena" | AoI_vector$name == "LasGuaitecas" | AoI_vector$name == "Kawesqar"], na.rm = TRUE)
  
  # BUILD PU DATAFRAME (Isla Magdalena)
  pu_data <- data.frame(id = AoI_vector$id[AoI_vector$name == "IslaMagdalena"],
                        monitoring_cost = 1,
                        status = 0)
  
  # BUILD FEATURE DATAFRAME (Isla Magdalena)
  features_data <- data.frame(id = 1,
                              target_recovery = r * sum(AoI_vector$TOTAL_RISK_Ecosystem[AoI_vector$name == "IslaMagdalena"], na.rm = TRUE),
                              name = "Kelp_Forest")
  
  # BUILD FEATURE DISTRIBUTION DATAFRAME (Isla Magdalena)
  dist_features_data <- data.frame(pu = AoI_vector$id[AoI_vector$name == "IslaMagdalena"],
                                   amount = AoI_vector$TOTAL_RISK_Ecosystem[AoI_vector$name == "IslaMagdalena"],
                                   feature = 1)
  dist_features_data <- dist_features_data[dist_features_data$amount > 0,]
  
  # BUILD THREAT DATAFRAME (Isla Magdalena)
  threats_data <- data.frame(id = 1,
                             blm_action = 0,
                             name = "FROM ALL STRESSORS")
  
  # BUILD THREAT DISTRIBUTION DATAFRAME (Isla Magdalena)
  dist_threats_data <- data.frame(pu = dist_features_data$pu,
                                  threat = 1,
                                  amount = 1,
                                  action_cost = 1,
                                  status = 0)
  
  # BUILD SENSITIVITY DATAFRAME (Isla Magdalena)
  sensitivity_data <- data.frame(feature = 1,
                                 threat = 1)
  
  # BUILD BOUNDARY DATAFRAME (Isla Magdalena)
  boundary_data <- data.frame(id1 = boundary$InputID,
                              id2 = boundary$TargetID,
                              boundary = boundary$kNN)
  boundary_data <- boundary_data[boundary_data$boundary > 0,]
  
  # CREATE PROBLEM (Isla Magdalena)
  d <- inputData(pu = pu_data,
                 features = features_data,
                 dist_features = dist_features_data,
                 threats = threats_data,
                 dist_threats = dist_threats_data,
                 sensitivity = sensitivity_data,
                 boundary = boundary_data)
  
  # CREATE MODEL (Isla Magdalena)
  p <- problem(d, model_type = "minimizeCosts", blm = 20)
  
  # SOLVE MODEL (Isla Magdalena)
  s <- solve(p, solver = "gurobi", cores = 8, verbose = TRUE, output_file = FALSE)
  
  # SAVE SOLUTIONS
  output_solutionsmatrix$disaggregate <- 0
  output_solutionsmatrix$disaggregate[output_solutionsmatrix$name == "IslaMagdalena"] <- replace(getActions(s)$"1", getActions(s)$"1" == 1, 2) + getActions(s)$connectivity
  
  # SAVE PERFORMANCE INDICATORS (Isla Magdalena)
  output_sum$disaggregate <- 0
  
  output_sum$disaggregate[output_sum$subregions == "IslaMagdalena" & output_sum$performance_index == "Actions"] <- sum(getActions(s)$"1")
  output_sum$disaggregate[output_sum$subregions == "IslaMagdalena" & output_sum$performance_index == "Monitoring"] <- sum(getActions(s)$connectivity)
  output_sum$disaggregate[output_sum$subregions == "IslaMagdalena" & output_sum$performance_index == "RiskReduction"] <- getSolutionBenefit(s)$benefit.total
  output_sum$disaggregate[output_sum$subregions == "IslaMagdalena" & output_sum$performance_index == "RiskReduction%"] <- 100 * (getSolutionBenefit(s)$benefit.total / (r * R))
  
  # BUILD PU DATAFRAME (Las Guaitecas)
  pu_data <- data.frame(id = AoI_vector$id[AoI_vector$name == "LasGuaitecas"],
                        monitoring_cost = 1,
                        status = 0)
  
  # BUILD FEATURE DATAFRAME (Las Guaitecas)
  features_data <- data.frame(id = 1,
                              target_recovery = r * sum(AoI_vector$TOTAL_RISK_Ecosystem[AoI_vector$name == "LasGuaitecas"], na.rm = TRUE),
                              name = "Kelp_Forest")
  
  # BUILD FEATURE DISTRIBUTION DATAFRAME (Las Guaitecas)
  dist_features_data <- data.frame(pu = AoI_vector$id[AoI_vector$name == "LasGuaitecas"],
                                   amount = AoI_vector$TOTAL_RISK_Ecosystem[AoI_vector$name == "LasGuaitecas"],
                                   feature = 1)
  dist_features_data <- dist_features_data[dist_features_data$amount > 0,]
  
  # BUILD THREAT DATAFRAME (Las Guaitecas)
  threats_data <- data.frame(id = 1,
                             blm_action = 0,
                             name = "FROM ALL STRESSORS")
  
  # BUILD THREAT DISTRIBUTION DATAFRAME (Las Guaitecas)
  dist_threats_data <- data.frame(pu = dist_features_data$pu,
                                  threat = 1,
                                  amount = 1,
                                  action_cost = 1,
                                  status = 0)
  
  # BUILD SENSITIVITY DATAFRAME (Las Guaitecas)
  sensitivity_data <- data.frame(feature = 1,
                                 threat = 1)
  
  # BUILD BOUNDARY DATAFRAME (Las Guaitecas)
  boundary_data <- data.frame(id1 = boundary$InputID,
                              id2 = boundary$TargetID,
                              boundary = boundary$kNN)
  boundary_data <- boundary_data[boundary_data$boundary > 0,]
  
  # CREATE PROBLEM (Las Guaitecas)
  d <- inputData(pu = pu_data,
                 features = features_data,
                 dist_features = dist_features_data,
                 threats = threats_data,
                 dist_threats = dist_threats_data,
                 sensitivity = sensitivity_data,
                 boundary = boundary_data)
  
  # CREATE MODEL (Las Guaitecas)
  p <- problem(d, model_type = "minimizeCosts", blm = 20)
  
  # SOLVE MODEL (Las Guaitecas)
  s <- solve(p, solver = "gurobi", cores = 8, verbose = TRUE, output_file = FALSE)
  
  # SAVE SOLUTIONS
  output_solutionsmatrix$disaggregate[output_solutionsmatrix$name == "LasGuaitecas"] <- replace(getActions(s)$"1", getActions(s)$"1" == 1, 2) + getActions(s)$connectivity
  
  # SAVE PERFORMANCE INDICATORS (Las Guaitecas)
  output_sum$disaggregate[output_sum$subregions == "LasGuaitecas" & output_sum$performance_index == "Actions"] <- sum(getActions(s)$"1")
  output_sum$disaggregate[output_sum$subregions == "LasGuaitecas" & output_sum$performance_index == "Monitoring"] <- sum(getActions(s)$connectivity)
  output_sum$disaggregate[output_sum$subregions == "LasGuaitecas" & output_sum$performance_index == "RiskReduction"] <- getSolutionBenefit(s)$benefit.total
  output_sum$disaggregate[output_sum$subregions == "LasGuaitecas" & output_sum$performance_index == "RiskReduction%"] <- 100 * (getSolutionBenefit(s)$benefit.total / (r * R))
  
  # BUILD PU DATAFRAME (Kawésqar)
  pu_data <- data.frame(id = AoI_vector$id[AoI_vector$name == "Kawesqar"],
                        monitoring_cost = 1,
                        status = 0)
  
  # BUILD FEATURE DATAFRAME (Kawésqar)
  features_data <- data.frame(id = 1,
                              target_recovery = r * sum(AoI_vector$TOTAL_RISK_Ecosystem[AoI_vector$name == "Kawesqar"], na.rm = TRUE),
                              name = "Kelp_Forest")
  
  # BUILD FEATURE DISTRIBUTION DATAFRAME (Kawésqar)
  dist_features_data <- data.frame(pu = AoI_vector$id[AoI_vector$name == "Kawesqar"],
                                   amount = AoI_vector$TOTAL_RISK_Ecosystem[AoI_vector$name == "Kawesqar"],
                                   feature = 1)
  dist_features_data <- dist_features_data[dist_features_data$amount > 0,]
  
  # BUILD THREAT DATAFRAME (Kawésqar)
  threats_data <- data.frame(id = 1,
                             blm_action = 0,
                             name = "FROM ALL STRESSORS")
  
  # BUILD THREAT DISTRIBUTION DATAFRAME (Kawésqar)
  dist_threats_data <- data.frame(pu = dist_features_data$pu,
                                  threat = 1,
                                  amount = 1,
                                  action_cost = 1,
                                  status = 0)
  
  # BUILD SENSITIVITY DATAFRAME (Kawésqar)
  sensitivity_data <- data.frame(feature = 1,
                                 threat = 1)
  
  # BUILD BOUNDARY DATAFRAME (Kawésqar)
  boundary_data <- data.frame(id1 = boundary$InputID,
                              id2 = boundary$TargetID,
                              boundary = boundary$kNN)
  boundary_data <- boundary_data[boundary_data$boundary > 0,]
  
  # CREATE PROBLEM (Kawésqar)
  d <- inputData(pu = pu_data,
                 features = features_data,
                 dist_features = dist_features_data,
                 threats = threats_data,
                 dist_threats = dist_threats_data,
                 sensitivity = sensitivity_data,
                 boundary = boundary_data)
  
  # CREATE MODEL (Kawésqar)
  p <- problem(d, model_type = "minimizeCosts", blm = 20)
  
  # SOLVE MODEL (Kawésqar)
  s <- solve(p, solver = "gurobi", cores = 8, verbose = TRUE, output_file = FALSE)
  
  # SAVE SOLUTIONS
  output_solutionsmatrix$disaggregate[output_solutionsmatrix$name == "Kawesqar"] <- replace(getActions(s)$"1", getActions(s)$"1" == 1, 2) + getActions(s)$connectivity
  
  # SAVE PERFORMANCE INDICATORS (Kawésqar)
  output_sum$disaggregate[output_sum$subregions == "Kawesqar" & output_sum$performance_index == "Actions"] <- sum(getActions(s)$"1")
  output_sum$disaggregate[output_sum$subregions == "Kawesqar" & output_sum$performance_index == "Monitoring"] <- sum(getActions(s)$connectivity)
  output_sum$disaggregate[output_sum$subregions == "Kawesqar" & output_sum$performance_index == "RiskReduction"] <- getSolutionBenefit(s)$benefit.total
  output_sum$disaggregate[output_sum$subregions == "Kawesqar" & output_sum$performance_index == "RiskReduction%"] <- 100 * (getSolutionBenefit(s)$benefit.total / (r * R))
  
  # SAVE PERFORMANCE INDICATORS (ChileanPatagonia)
  output_sum$disaggregate[output_sum$subregions == "ChileanPatagonia" & output_sum$performance_index == "Actions"] <- sum(output_sum$disaggregate[output_sum$subregions != "ChileanPatagonia" & output_sum$performance_index == "Actions"])
  output_sum$disaggregate[output_sum$subregions == "ChileanPatagonia" & output_sum$performance_index == "Monitoring"] <- sum(output_sum$disaggregate[output_sum$subregions != "ChileanPatagonia" & output_sum$performance_index == "Monitoring"])
  output_sum$disaggregate[output_sum$subregions == "ChileanPatagonia" & output_sum$performance_index == "RiskReduction"] <- sum(output_sum$disaggregate[output_sum$subregions != "ChileanPatagonia" & output_sum$performance_index == "RiskReduction"])
  output_sum$disaggregate[output_sum$subregions == "ChileanPatagonia" & output_sum$performance_index == "RiskReduction%"] <- sum(output_sum$disaggregate[output_sum$subregions != "ChileanPatagonia" & output_sum$performance_index == "RiskReduction%"])
  
  colnames(output_sum)[(colnames(output_sum) == "disaggregate")] <- paste0("disaggregate_", r)
  colnames(output_solutionsmatrix)[(colnames(output_solutionsmatrix) == "disaggregate")] <- paste0("disaggregate_", r)
  
  ##############################################################################
  # TRADITIONAL APPROACH
  ##############################################################################
  
  t <- r * nrow(AoI_vector)
  R <- sum(AoI_vector$TOTAL_RISK_Ecosystem, na.rm = TRUE)
  
  # NO CONNECTED SCENARIO ------------------------------------------------------
  
  # BUILD PU DATAFRAME
  pu_data <- data.frame(id = AoI_vector$id,
                        monitoring_cost = 1,
                        status = 0)
  
  # BUILD FEATURE DATAFRAME
  features_data <- data.frame(id = 1,
                              target_recovery = t,
                              name = "Kelp_Forest")
  
  # BUILD FEATURE DISTRIBUTION DATAFRAME
  dist_features_data <- data.frame(pu = AoI_vector$id,
                                   amount = 1,
                                   feature = 1)
  
  # BUILD THREAT DATAFRAME
  threats_data <- data.frame(id = 1,
                             blm_action = 0,
                             name = "FROM ALL STRESSORS")
  
  # BUILD THREAT DISTRIBUTION DATAFRAME
  dist_threats_data <- data.frame(pu = dist_features_data$pu,
                                  threat = 1,
                                  amount = 1,
                                  action_cost = AoI_vector$TOTAL_RISK_Ecosystem,
                                  status = 0)
  
  # BUILD SENSITIVITY DATAFRAME
  sensitivity_data <- data.frame(feature = 1,
                                 threat = 1)
  
  # CREATE PROBLEM
  d <- inputData(pu = pu_data,
                 features = features_data,
                 dist_features = dist_features_data,
                 threats = threats_data,
                 dist_threats = dist_threats_data,
                 sensitivity = sensitivity_data)
  
  # CREATE MODEL
  p <- problem(d, model_type = "minimizeCosts", blm = 0)
  
  # SOLVE MODEL
  s <- solve(p, solver = "gurobi", cores = 8, verbose = TRUE, output_file = FALSE)
  
  # SAVE SOLUTIONS
  output_solutionsmatrix$no_connect_TA <- replace(getActions(s)$"1", getActions(s)$"1" == 1, 2) + getActions(s)$connectivity
  
  # SAVE PERFORMANCE INDICATORS
  no_connect_TA_perf_idx <- c()
  
  # Chilean Patagonia
  no_connect_TA_perf_idx <- c(no_connect_TA_perf_idx, sum(getActions(s)$"1"))
  no_connect_TA_perf_idx <- c(no_connect_TA_perf_idx, sum(getActions(s)$connectivity))
  no_connect_TA_perf_idx <- c(no_connect_TA_perf_idx, sum(getActions(s)$"1" * AoI_vector$TOTAL_RISK_Ecosystem))
  no_connect_TA_perf_idx <- c(no_connect_TA_perf_idx, 100 * (sum(getActions(s)$"1" * AoI_vector$TOTAL_RISK_Ecosystem) / (r * R)))
  
  # Los Lagos
  no_connect_TA_perf_idx <- c(no_connect_TA_perf_idx, sum(output_solutionsmatrix$no_connect_TA[output_solutionsmatrix$name == "LosLagos" & output_solutionsmatrix$no_connect_TA == 2]) / 2)
  no_connect_TA_perf_idx <- c(no_connect_TA_perf_idx, sum(output_solutionsmatrix$no_connect_TA[output_solutionsmatrix$name == "LosLagos" & output_solutionsmatrix$no_connect_TA == 1]))
  no_connect_TA_perf_idx <- c(no_connect_TA_perf_idx, sum(output_solutionsmatrix$TOTAL_RISK_Ecosystem[output_solutionsmatrix$name == "LosLagos" & output_solutionsmatrix$no_connect_TA == 2]))
  no_connect_TA_perf_idx <- c(no_connect_TA_perf_idx, 100 * sum(output_solutionsmatrix$TOTAL_RISK_Ecosystem[output_solutionsmatrix$name == "LosLagos" & output_solutionsmatrix$no_connect_TA == 2]) / (r * R))
  
  # Isla Magdalena
  no_connect_TA_perf_idx <- c(no_connect_TA_perf_idx, sum(output_solutionsmatrix$no_connect_TA[output_solutionsmatrix$name == "IslaMagdalena" & output_solutionsmatrix$no_connect_TA == 2]) / 2)
  no_connect_TA_perf_idx <- c(no_connect_TA_perf_idx, sum(output_solutionsmatrix$no_connect_TA[output_solutionsmatrix$name == "IslaMagdalena" & output_solutionsmatrix$no_connect_TA == 1]))
  no_connect_TA_perf_idx <- c(no_connect_TA_perf_idx, sum(output_solutionsmatrix$TOTAL_RISK_Ecosystem[output_solutionsmatrix$name == "IslaMagdalena" & output_solutionsmatrix$no_connect_TA == 2]))
  no_connect_TA_perf_idx <- c(no_connect_TA_perf_idx, 100 * sum(output_solutionsmatrix$TOTAL_RISK_Ecosystem[output_solutionsmatrix$name == "IslaMagdalena" & output_solutionsmatrix$no_connect_TA == 2]) / (r * R))
  
  # Las Guaitecas
  no_connect_TA_perf_idx <- c(no_connect_TA_perf_idx, sum(output_solutionsmatrix$no_connect_TA[output_solutionsmatrix$name == "LasGuaitecas" & output_solutionsmatrix$no_connect_TA == 2]) / 2)
  no_connect_TA_perf_idx <- c(no_connect_TA_perf_idx, sum(output_solutionsmatrix$no_connect_TA[output_solutionsmatrix$name == "LasGuaitecas" & output_solutionsmatrix$no_connect_TA == 1]))
  no_connect_TA_perf_idx <- c(no_connect_TA_perf_idx, sum(output_solutionsmatrix$TOTAL_RISK_Ecosystem[output_solutionsmatrix$name == "LasGuaitecas" & output_solutionsmatrix$no_connect_TA == 2]))
  no_connect_TA_perf_idx <- c(no_connect_TA_perf_idx, 100 * sum(output_solutionsmatrix$TOTAL_RISK_Ecosystem[output_solutionsmatrix$name == "LasGuaitecas" & output_solutionsmatrix$no_connect_TA == 2]) / (r * R))
  
  # Kawésqar
  no_connect_TA_perf_idx <- c(no_connect_TA_perf_idx, sum(output_solutionsmatrix$no_connect_TA[output_solutionsmatrix$name == "Kawesqar" & output_solutionsmatrix$no_connect_TA == 2]) / 2)
  no_connect_TA_perf_idx <- c(no_connect_TA_perf_idx, sum(output_solutionsmatrix$no_connect_TA[output_solutionsmatrix$name == "Kawesqar" & output_solutionsmatrix$no_connect_TA == 1]))
  no_connect_TA_perf_idx <- c(no_connect_TA_perf_idx, sum(output_solutionsmatrix$TOTAL_RISK_Ecosystem[output_solutionsmatrix$name == "Kawesqar" & output_solutionsmatrix$no_connect_TA == 2]))
  no_connect_TA_perf_idx <- c(no_connect_TA_perf_idx, 100 * sum(output_solutionsmatrix$TOTAL_RISK_Ecosystem[output_solutionsmatrix$name == "Kawesqar" & output_solutionsmatrix$no_connect_TA == 2]) / (r * R))
  
  # Other
  no_connect_TA_perf_idx <- c(no_connect_TA_perf_idx, sum(output_solutionsmatrix$no_connect_TA[output_solutionsmatrix$name == "Other" & output_solutionsmatrix$no_connect_TA == 2]) / 2)
  no_connect_TA_perf_idx <- c(no_connect_TA_perf_idx, sum(output_solutionsmatrix$no_connect_TA[output_solutionsmatrix$name == "Other" & output_solutionsmatrix$no_connect_TA == 1]))
  no_connect_TA_perf_idx <- c(no_connect_TA_perf_idx, sum(output_solutionsmatrix$TOTAL_RISK_Ecosystem[output_solutionsmatrix$name == "Other" & output_solutionsmatrix$no_connect_TA == 2]))
  no_connect_TA_perf_idx <- c(no_connect_TA_perf_idx, 100 * sum(output_solutionsmatrix$TOTAL_RISK_Ecosystem[output_solutionsmatrix$name == "Other" & output_solutionsmatrix$no_connect_TA == 2]) / (r * R))
  
  output_sum$no_connect_TA <- no_connect_TA_perf_idx
  colnames(output_sum)[(colnames(output_sum) == "no_connect_TA")] <- paste0("no_connect_", r, "_TA")
  colnames(output_solutionsmatrix)[(colnames(output_solutionsmatrix) == "no_connect_TA")] <- paste0("no_connect_", r, "_TA")
  
  # CONNECTED SCENARIO ---------------------------------------------------------
  
  # BUILD PU DATAFRAME
  pu_data <- data.frame(id = AoI_vector$id,
                        monitoring_cost = 1,
                        status = 0)
  
  # BUILD FEATURE DATAFRAME
  features_data <- data.frame(id = 1,
                              target_recovery = t,
                              name = "Kelp_Forest")
  
  # BUILD FEATURE DISTRIBUTION DATAFRAME
  dist_features_data <- data.frame(pu = AoI_vector$id,
                                   amount = 1,
                                   feature = 1)
  
  # BUILD THREAT DATAFRAME
  threats_data <- data.frame(id = 1,
                             blm_action = 0,
                             name = "FROM ALL STRESSORS")
  
  # BUILD THREAT DISTRIBUTION DATAFRAME
  dist_threats_data <- data.frame(pu = dist_features_data$pu,
                                  threat = 1,
                                  amount = 1,
                                  action_cost = AoI_vector$TOTAL_RISK_Ecosystem,
                                  status = 0)
  
  # BUILD SENSITIVITY DATAFRAME
  sensitivity_data <- data.frame(feature = 1,
                                 threat = 1)
  
  # BUILD BOUNDARY DATAFRAME
  boundary_data <- data.frame(id1 = boundary$InputID,
                              id2 = boundary$TargetID,
                              boundary = boundary$kNN)
  boundary_data <- boundary_data[boundary_data$boundary > 0,]
  
  # CREATE PROBLEM
  d <- inputData(pu = pu_data,
                 features = features_data,
                 dist_features = dist_features_data,
                 threats = threats_data,
                 dist_threats = dist_threats_data,
                 sensitivity = sensitivity_data,
                 boundary = boundary_data)
  
  # CREATE MODEL
  p <- problem(d, model_type = "minimizeCosts", blm = 20)
  
  # SOLVE MODEL
  s <- solve(p, solver = "gurobi", cores = 8, verbose = TRUE, output_file = FALSE)
  
  # SAVE SOLUTIONS
  output_solutionsmatrix$connect_TA <- replace(getActions(s)$"1", getActions(s)$"1" == 1, 2) + getActions(s)$connectivity
  
  # SAVE PERFORMANCE INDICATORS
  connect_TA_perf_idx <- c()
  
  # Chilean Patagonia
  connect_TA_perf_idx <- c(connect_TA_perf_idx, sum(getActions(s)$"1"))
  connect_TA_perf_idx <- c(connect_TA_perf_idx, sum(getActions(s)$connectivity))
  connect_TA_perf_idx <- c(connect_TA_perf_idx, sum(getActions(s)$"1" * AoI_vector$TOTAL_RISK_Ecosystem))
  connect_TA_perf_idx <- c(connect_TA_perf_idx, 100 * (sum(getActions(s)$"1" * AoI_vector$TOTAL_RISK_Ecosystem) / (r * R)))
  
  # Los Lagos
  connect_TA_perf_idx <- c(connect_TA_perf_idx, sum(output_solutionsmatrix$connect_TA[output_solutionsmatrix$name == "LosLagos" & output_solutionsmatrix$connect_TA == 2]) / 2)
  connect_TA_perf_idx <- c(connect_TA_perf_idx, sum(output_solutionsmatrix$connect_TA[output_solutionsmatrix$name == "LosLagos" & output_solutionsmatrix$connect_TA == 1]))
  connect_TA_perf_idx <- c(connect_TA_perf_idx, sum(output_solutionsmatrix$TOTAL_RISK_Ecosystem[output_solutionsmatrix$name == "LosLagos" & output_solutionsmatrix$connect_TA == 2]))
  connect_TA_perf_idx <- c(connect_TA_perf_idx, 100 * sum(output_solutionsmatrix$TOTAL_RISK_Ecosystem[output_solutionsmatrix$name == "LosLagos" & output_solutionsmatrix$connect_TA == 2]) / (r * R))
  
  # Isla Magdalena
  connect_TA_perf_idx <- c(connect_TA_perf_idx, sum(output_solutionsmatrix$connect_TA[output_solutionsmatrix$name == "IslaMagdalena" & output_solutionsmatrix$connect_TA == 2]) / 2)
  connect_TA_perf_idx <- c(connect_TA_perf_idx, sum(output_solutionsmatrix$connect_TA[output_solutionsmatrix$name == "IslaMagdalena" & output_solutionsmatrix$connect_TA == 1]))
  connect_TA_perf_idx <- c(connect_TA_perf_idx, sum(output_solutionsmatrix$TOTAL_RISK_Ecosystem[output_solutionsmatrix$name == "IslaMagdalena" & output_solutionsmatrix$connect_TA == 2]))
  connect_TA_perf_idx <- c(connect_TA_perf_idx, 100 * sum(output_solutionsmatrix$TOTAL_RISK_Ecosystem[output_solutionsmatrix$name == "IslaMagdalena" & output_solutionsmatrix$connect_TA == 2]) / (r * R))
  
  # Las Guaitecas
  connect_TA_perf_idx <- c(connect_TA_perf_idx, sum(output_solutionsmatrix$connect_TA[output_solutionsmatrix$name == "LasGuaitecas" & output_solutionsmatrix$connect_TA == 2]) / 2)
  connect_TA_perf_idx <- c(connect_TA_perf_idx, sum(output_solutionsmatrix$connect_TA[output_solutionsmatrix$name == "LasGuaitecas" & output_solutionsmatrix$connect_TA == 1]))
  connect_TA_perf_idx <- c(connect_TA_perf_idx, sum(output_solutionsmatrix$TOTAL_RISK_Ecosystem[output_solutionsmatrix$name == "LasGuaitecas" & output_solutionsmatrix$connect_TA == 2]))
  connect_TA_perf_idx <- c(connect_TA_perf_idx, 100 * sum(output_solutionsmatrix$TOTAL_RISK_Ecosystem[output_solutionsmatrix$name == "LasGuaitecas" & output_solutionsmatrix$connect_TA == 2]) / (r * R))
  
  # Kawésqar
  connect_TA_perf_idx <- c(connect_TA_perf_idx, sum(output_solutionsmatrix$connect_TA[output_solutionsmatrix$name == "Kawesqar" & output_solutionsmatrix$connect_TA == 2]) / 2)
  connect_TA_perf_idx <- c(connect_TA_perf_idx, sum(output_solutionsmatrix$connect_TA[output_solutionsmatrix$name == "Kawesqar" & output_solutionsmatrix$connect_TA == 1]))
  connect_TA_perf_idx <- c(connect_TA_perf_idx, sum(output_solutionsmatrix$TOTAL_RISK_Ecosystem[output_solutionsmatrix$name == "Kawesqar" & output_solutionsmatrix$connect_TA == 2]))
  connect_TA_perf_idx <- c(connect_TA_perf_idx, 100 * sum(output_solutionsmatrix$TOTAL_RISK_Ecosystem[output_solutionsmatrix$name == "Kawesqar" & output_solutionsmatrix$connect_TA == 2]) / (r * R))
  
  # Other
  connect_TA_perf_idx <- c(connect_TA_perf_idx, sum(output_solutionsmatrix$connect_TA[output_solutionsmatrix$name == "Other" & output_solutionsmatrix$connect_TA == 2]) / 2)
  connect_TA_perf_idx <- c(connect_TA_perf_idx, sum(output_solutionsmatrix$connect_TA[output_solutionsmatrix$name == "Other" & output_solutionsmatrix$connect_TA == 1]))
  connect_TA_perf_idx <- c(connect_TA_perf_idx, sum(output_solutionsmatrix$TOTAL_RISK_Ecosystem[output_solutionsmatrix$name == "Other" & output_solutionsmatrix$connect_TA == 2]))
  connect_TA_perf_idx <- c(connect_TA_perf_idx, 100 * sum(output_solutionsmatrix$TOTAL_RISK_Ecosystem[output_solutionsmatrix$name == "Other" & output_solutionsmatrix$connect_TA == 2]) / (r * R))
  
  output_sum$connect_TA <- connect_TA_perf_idx
  colnames(output_sum)[(colnames(output_sum) == "connect_TA")] <- paste0("connect_", r, "_TA")
  colnames(output_solutionsmatrix)[(colnames(output_solutionsmatrix) == "connect_TA")] <- paste0("connect_", r, "_TA")
  
  # SELECTION FREQUENCY
  columns <- grep(paste0(r, "$"), colnames(output_solutionsmatrix))
  output_solutionsmatrix$select_freq <- rowSums(output_solutionsmatrix[, columns] != 0)
  colnames(output_solutionsmatrix)[(colnames(output_solutionsmatrix) == "select_freq")] <- paste0("select_freq_", r)
}

columns <- grep("^select_freq", colnames(output_solutionsmatrix))
if (length(columns) > 1) {
  output_solutionsmatrix$select_freq_ALL <- rowSums(output_solutionsmatrix[, columns])
}

# WRITE OUTPUTS
write.csv(output_solutionsmatrix, file = "output_solutionsmatrix.csv", row.names = FALSE)
write.csv(output_sum, file = "output_sum.csv", row.names = FALSE)

# PLOT SOLUTIONS MATRIX --------------------------------------------------------
plot_solutionsmatrix <- function(extent_file, crs, shp1, shp2, column_name, palette, extent_name) {
  if (!dir.exists("output_plots")) {
    dir.create("output_plots")
  }
  
  if (palette != "Reds") {
    shp2[[column_name]][shp2[[column_name]] == 0] <- NA
  }
  
  extent_shp <- st_read(extent_file)
  extent_shp <- st_transform(extent_shp, crs = crs)
  
  centroid <- st_centroid(st_union(extent_shp))
  centroid_coords <- st_coordinates(centroid)
  
  bbox <- st_bbox(extent_shp)
  
  ymin <- bbox["ymin"]
  ymax <- bbox["ymax"]
  
  height <- ymax - ymin
  width <- (4 / 3) * height
  
  xmin <- centroid_coords[1] - (width / 2)
  xmax <- centroid_coords[1] + (width / 2)
  ymin <- centroid_coords[2] - (height / 2)
  ymax <- centroid_coords[2] + (height / 2)
  
  bbox <- data.frame(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
  
  plot <- ggplot() +
    geom_sf(data = shp1, fill = NA, color = "black") +
    geom_sf(data = shp2, aes(fill = !!sym(column_name)), color = NA) +
    coord_sf(xlim = c(bbox$xmin, bbox$xmax), ylim = c(bbox$ymin, bbox$ymax), expand = FALSE) +
    theme_minimal() +
    theme(legend.position = "none", axis.text = element_blank(), axis.ticks = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
    if (palette == "Reds") {
      plot <- plot + scale_fill_distiller(palette = palette, direction = 1)
    } else {
      max_limit <- max(shp2[[column_name]], na.rm = TRUE)
      plot <- plot + scale_fill_distiller(palette = palette, direction = 1, na.value = "transparent", limits = c(0, max_limit))
    }
  
  output_filename <- paste0("./output_plots/", column_name, "_", extent_name, ".png")
  ggsave(output_filename, plot, width = 12, height = 9, dpi = 300, bg = "white", units = "in", limitsize = FALSE)
}

solutionsmatrix <- read.csv("output_solutionsmatrix.csv", sep=",")

CL <- sf::st_read("./Regiones/regiones.shp")
AoI_vector <- sf::st_read("./Input/subregions.shp")

CL <- CL[CL$objectid != 1100,]
CL <- CL[, 0]

crs <- 3857
CL <- sf::st_transform(CL, crs = crs)
AoI_vector <- sf::st_transform(AoI_vector, crs = crs)

AoI_vector <- merge(AoI_vector, solutionsmatrix[, c(1, 4:ncol(solutionsmatrix))], by = "id")

subregions <- list(
  "los_lagos" = "./chiloe-llanquihue-palena/chiloe-llanquihue-palena.shp",
  "isla_magdalena" = "./Protected_Areas/PN_Isla_Magdalena.shp",
  "las_guaitecas" = "./Protected_Areas/RN_Las_Guaitecas.shp",
  "kawesqar" = "./Protected_Areas/PN_Kawesqar.shp",
  "chilean_patagonia" = "./Input/subregions.shp"
)

# SAVE PLOT TOTAL RISK ECOSYSTEM
columns <- names(AoI_vector)[grep("^TOTAL_RISK_Ecosystem", names(AoI_vector))]

for (extent_name in names(subregions)) {
  extent_file <- subregions[[extent_name]]
  for (column_name in columns) {
    plot_solutionsmatrix(extent_file, crs, CL, AoI_vector, column_name, "Reds", extent_name)
  }
}

# SAVE PLOT MANAGEMENT PLANS
columns <- names(AoI_vector)[grep("^(no_connect|connect|locke_out|aggregate|disaggregate)", names(AoI_vector))]

for (extent_name in names(subregions)) {
  extent_file <- subregions[[extent_name]]
  for (column_name in columns) {
    plot_solutionsmatrix(extent_file, crs, CL, AoI_vector, column_name, "Greys", extent_name)
  }
}

# SAVE PLOT SELECTION FREQUENCY
columns <- names(AoI_vector)[grep("^select_freq", names(AoI_vector))]

for (extent_name in names(subregions)) {
  extent_file <- subregions[[extent_name]]
  for (column_name in columns) {
    plot_solutionsmatrix(extent_file, crs, CL, AoI_vector, column_name, "Greens", extent_name)
  }
}

################################################################################
# TRADITIONAL APPROACH VS RISK MANAGEMENT APPROACH
################################################################################

approach <- c()
scenario <- c()
cost_actions <- c()
cost_monitoring <- c()
risk_reduction <- c()
risk_reduction_percent <- c()

i <- 1
vec1 <- c(seq(0, 20, 20))
vec2 <- c(seq(0.10, 0.30, 0.05))
R <- sum(AoI_vector$TOTAL_RISK_Ecosystem, na.rm = TRUE)

# BUILD PU DATAFRAME
pu_data <- data.frame(id = AoI_vector$id,
                      monitoring_cost = 1,
                      status = 0)
# BUILD THREAT DATAFRAME
threats_data <- data.frame(id = 1,
                           blm_action = 0,
                           name = "FROM ALL STRESSORS")

# BUILD SENSITIVITY DATAFRAME
sensitivity_data <- data.frame(feature = 1,
                               threat = 1)

# BUILD BOUNDARY DATAFRAME
boundary_data <- data.frame(id1 = boundary$InputID,
                            id2 = boundary$TargetID,
                            boundary = boundary$kNN)
boundary_data <- boundary_data[boundary_data$boundary > 0,]

for(r in vec2){
  for(blm in vec1){
    # TRADITIONAL APPROACH -------------------------------------------------------
    t <- r * nrow(AoI_vector)
    
    # BUILD FEATURE DATAFRAME
    features_data <- data.frame(id = 1,
                                target_recovery = t,
                                name = "Kelp_Forest")
    
    # BUILD FEATURE DISTRIBUTION DATAFRAME
    dist_features_data <- data.frame(pu = AoI_vector$id,
                                     amount = 1,
                                     feature = 1)
    
    # BUILD THREAT DISTRIBUTION DATAFRAME
    dist_threats_data <- data.frame(pu = dist_features_data$pu,
                                    threat = 1,
                                    amount = 1,
                                    action_cost = AoI_vector$TOTAL_RISK_Ecosystem,
                                    status = 0)
    
    # CREATE PROBLEM
    d <- inputData(pu = pu_data,
                   features = features_data,
                   dist_features = dist_features_data,
                   threats = threats_data,
                   dist_threats = dist_threats_data,
                   sensitivity = sensitivity_data,
                   boundary = boundary_data)
    
    # CREATE MODEL
    p <- problem(d, model_type = "minimizeCosts", blm = blm)
    
    # SOLVE MODEL
    s <- solve(p, solver = "gurobi", cores = 8, verbose = TRUE, output_file = FALSE)
    
    # SAVE PERFORMANCE INDICATORS
    cost_actions[i] <- sum(getActions(s)$"1")
    cost_monitoring[i] <- sum(getActions(s)$connectivity)
    risk_reduction[i] <- sum(getActions(s)$"1" * AoI_vector$TOTAL_RISK_Ecosystem)
    risk_reduction_percent[i] <- 100 * (sum(getActions(s)$"1" * AoI_vector$TOTAL_RISK_Ecosystem) / (r * R))
    
    if(blm == 0) {
      scenario[i] <- paste0("(NC,0), ", 100 * r, "%")
    } else {
      scenario[i] <- paste0("(C,>0), ", 100 * r, "%")
    }
    
    approach[i] <- "Traditional"
    
    i <- i + 1
    
    # RISK MANAGEMENT APPROACH ---------------------------------------------------
    
    # BUILD FEATURE DATAFRAME
    features_data <- data.frame(id = 1,
                                target_recovery = r * R,
                                name = "Kelp_Forest")
    
    # BUILD FEATURE DISTRIBUTION DATAFRAME
    dist_features_data <- data.frame(pu = AoI_vector$id,
                                     amount = AoI_vector$TOTAL_RISK_Ecosystem,
                                     feature = 1)
    dist_features_data <- dist_features_data[dist_features_data$amount > 0,]
    
    # BUILD THREAT DISTRIBUTION DATAFRAME
    dist_threats_data <- data.frame(pu = dist_features_data$pu,
                                    threat = 1,
                                    amount = 1,
                                    action_cost = 1,
                                    status = 0)
    
    # CREATE PROBLEM
    d <- inputData(pu = pu_data,
                   features = features_data,
                   dist_features = dist_features_data,
                   threats = threats_data,
                   dist_threats = dist_threats_data,
                   sensitivity = sensitivity_data,
                   boundary = boundary_data)
    
    # CREATE MODEL
    p <- problem(d, model_type = "minimizeCosts", blm = blm)
    
    # SOLVE MODEL
    s <- solve(p, solver = "gurobi", cores = 8, verbose = TRUE, output_file = FALSE)
    
    # SAVE PERFORMANCE INDICATORS
    cost_actions[i] <- sum(getActions(s)$"1")
    cost_monitoring[i] <- sum(getActions(s)$connectivity)
    risk_reduction[i] <- getSolutionBenefit(s)$benefit.total
    risk_reduction_percent[i] <- 100 * (getSolutionBenefit(s)$benefit.total / (r * R))
    
    if(blm == 0) {
      scenario[i] <- paste0("(NC,0), ", 100 * r, "%")
    } else {
      scenario[i] <- paste0("(C,>0), ", 100 * r, "%")
    }
    
    approach[i] <- "Risk Management"
    
    i <- i + 1
  }
}

# CREATE RISK REDUCTION% DATAFRAME
risk_reduction_data <- data.frame(approach = approach,
                                  scenario = scenario,
                                  risk_reduction = risk_reduction,
                                  risk_reduction_percent = ifelse(risk_reduction_percent >= 100, "", paste0(round(risk_reduction_percent, 2), "%")))

write.csv(risk_reduction_data, file = "output_tradvsrisk.csv", row.names = FALSE)

# PLOT RISK REDUCTION% DATAFRAME
risk_reduction_data$approach <- factor(risk_reduction_data$approach, levels = unique(approach))
risk_reduction_data$scenario <- factor(risk_reduction_data$scenario, levels = unique(scenario))

ggplot(data = risk_reduction_data, aes(x = scenario, y = risk_reduction, fill = approach)) +
  geom_bar(stat = "identity", position = position_dodge(), width = .75) +
  geom_text(aes(label = risk_reduction_percent), vjust = -1.25, color = "black", position = position_dodge(1.), size = 3.5) +
  # scale_fill_brewer(palette = "Dark2") +
  scale_fill_manual(values = c(rgb(255, 230, 220, maxColorValue = 255), rgb(200, 20, 30, maxColorValue = 255))) +
  guides(fill = guide_legend(title = 'Approach')) +
  labs(x = "", y = "Risk Reduction") +
  theme_minimal()
  # coord_flip()

ggsave("trad_app_vs_risk_man.png", width = 6.1, height = 3.65, dpi = 1200)

################################################################################
# BOUNDARY LENGTH MODIFIER (BLM) VS MANAGEMENT COSTS
################################################################################

cost <- c()

i <- 1
vec <- c(seq(0, 50, 5))

for(blm in vec){
  # CREATE MODEL
  p <- problem(d, model_type = "minimizeCosts", blm = blm)
  
  # SOLVE MODEL
  s <- solve(p, solver = "gurobi", time_limit = 300, cores = 8, verbose = TRUE, name_output_file = paste0("output_blm_", blm), output_file = FALSE)
  
  # SAVE MANAGEMENT COSTS
  cost[i] <- sum(getActions(s)$"1") + sum(getActions(s)$connectivity)
  
  i <- i + 1
}

# CREATE COST DATAFRAME
cost_data <- data.frame(blm = vec,
                        selected_units = cost)

# PLOT COST DATAFRAME
plot(cost_data)

################################################################################
# MEAN SOLVING TIME
################################################################################

runtime <- c()

for(i in 1:30){
  # SOLVE MODEL
  s <- solve(p, solver = "gurobi", cores = 8, verbose = TRUE, output_file = FALSE)
  
  # SAVE RUNTIME
  runtime[i] <- getPerformance(s)$solving_time
  
  rm(s)
}

################################################################################
# RISK REDUCTION TARGET VS PERFORMANCE INDICATORS
################################################################################

cost_actions <- c()
cost_monitoring <- c()
risk_reduction <- c()
risk_reduction_percent <- c()

i <- 1
vec <- c(seq(0.05, 0.50, 0.05))

for(r in vec){
  # BUILD FEATURE DATAFRAME
  features_data <- data.frame(id = 1,
                              target_recovery = r * sum(AoI_vector$TOTAL_RISK_Ecosystem, na.rm = TRUE),
                              name = "Kelp_Forest")
  
  # CREATE PROBLEM
  d <- inputData(pu = pu_data,
                 features = features_data,
                 dist_features = dist_features_data,
                 threats = threats_data,
                 dist_threats = dist_threats_data,
                 sensitivity = sensitivity_data,
                 boundary = boundary_data)
  
  # CREATE MODEL
  p <- problem(d, model_type = "minimizeCosts", blm = 0)
  
  # SOLVE MODEL
  s <- solve(p, solver = "gurobi", cores = 8, verbose = TRUE, output_file = FALSE)
  
  # SAVE PERFORMANCE INDICATORS
  cost_actions[i] <- sum(getActions(s)$"1")
  cost_monitoring[i] <- sum(getActions(s)$connectivity)
  risk_reduction[i] <- getSolutionBenefit(s)$benefit.total
  risk_reduction_percent[i] <- 100 * (getSolutionBenefit(s)$benefit.total / (r * R))
  
  i <- i + 1
}
