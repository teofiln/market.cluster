## code to prepare `params` dataset goes here

params <- list( seed = 12345,
                start_k = 2,
                end_k = 8,
                max_iter = 200,
                classification = "Hard",
                init_center = "Random",
                centering_method = "Means",
                distance = "Euclidean",
                km_nrep = 10,
                boot_rep = 10,
                data = data_df,
                km_model = list(),
                flexmix_model = list(),
                boot_km_model = list(),
                reps_km_model= list(),
                method = "kmeans")
    

usethis::use_data(params, internal = TRUE, overwrite = TRUE)
