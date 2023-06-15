## Births imputation



### Multiple imputation of education, ethnicity and parity --------------------


# Save the data
lapply(1989:2021, function(x){  write.csv(d[d$Year == x, ], file = paste0("Raw/Imputation/imp_", x, ".csv"))})

# Create the names vector
all_samples <- as.data.frame(list.files("Raw/Imputation/"), include.dirs = T)
seq_id_all <- seq_along(1:nrow(all_samples))

# --------

# Get the number of cores
no_cores <- detectCores(logical = T)

# Register number of Cluster
cl <- makeCluster(33)

# Export to clusters
clusterExport(cl, list('impute_births', 'all_samples'))

# -------

# Run the imputation
births_imputed <- parLapply(cl, seq_id_all, fun = impute_births)

# Combine the results
births_imputed <- do.call(rbind, births_imputed)