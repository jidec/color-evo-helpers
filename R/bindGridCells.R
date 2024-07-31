#df = readRDS("D:/GitProjects/inat-daily-activity-analysis/_targets/objects/obs_timed")
#df = filterContiguousUSA(df)
#cellsize_km = 250

# df must have:
# 1. latitude and longitude cols
bindGridCells <- function(df,cellsize_km=250){
    library(sf)
    library(dplyr)

    df$lat_temp <- df$latitude
    df$lon_temp <- df$longitude

    # Convert df to sf object
    sf_data <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326)

    # Create the grid
    grid <- st_make_grid(sf_data, cellsize = c(cellsize_km * 0.009, cellsize_km * 0.009), square = TRUE) %>%
        st_sf('ID' = 1:length(.), geometry = .)

    # Calculate the center (centroid) of each grid cell
    grid_centers <- st_centroid(grid)

    # Add centroids' coordinates to the grid data
    grid_with_centroids <- cbind(grid, st_coordinates(grid_centers))

    # Perform spatial join, joining sf_data with the nearest grid cell
    result <- st_join(sf_data, grid, left = TRUE)

    # Add the cell column based on the grid ID
    result$cell <- result$ID

    # Add the latitude and longitude of grid cell centers to result
    result <- result %>%
        mutate(
            cell_lat = grid_with_centroids$Y[result$ID],
            cell_lon = grid_with_centroids$X[result$ID]
        )

    result$latitude <- result$lat_temp
    result$longitude <- result$lon_temp

    return(data.frame(result))
}
