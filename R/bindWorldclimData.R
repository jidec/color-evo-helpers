# Define the bindWorldclimData function
bindWorldclimData <- function(df, worldclim_var="bio", var_num=1, new_var_colname="tmax", res = 10, path = 'D:/bioclim_data') {

    library(geodata)
    library(terra)
    library(dplyr)

    # Download WorldClim data for the specified variable and resolution
    worldclim_data <- worldclim_global(var = worldclim_var, res = res, path = path)

    selected_layer <- worldclim_data[[var_num]]

    # Convert the dataframe to a SpatVector
    points <- vect(df, geom = c("longitude", "latitude"), crs = crs(selected_layer))

    # Extract WorldClim data for the given points
    extracted_data <- extract(selected_layer, points)

    # Combine the original dataframe with the WorldClim data
    result_df <- cbind(df, extracted_data[, -1])  # Remove the ID column from the extraction results

    # Rename the last column
    new_name <- new_var_colname
    last_col_name <- names(result_df)[ncol(result_df)]
    result_df <- result_df %>% rename(!!new_name := all_of(last_col_name))

    return(result_df)
}
