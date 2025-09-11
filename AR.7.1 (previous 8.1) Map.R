
# ======================================================
# Map Additional to GAIN 1: Prepare World Map (Remove Arctic & Antarctica) # to work more on maps later
# ======================================================

# Load world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Remove Antarctica from the world dataset
world_filtered <- world %>%
  filter(!grepl("Antarctica", name))  # Exclude Antarctica

# Step 2: Create and Save the First Map (Overall Country-led Example)

# Filter data for all country-led examples in 2024
year_data_all <- group_roster %>%
  filter(ryear == 2024, g_conled == 1) %>%
  group_by(mcountry) %>%
  summarise(Count = n(), .groups = "drop")

# Merge with filtered world map data
year_data_all <- left_join(year_data_all, world_filtered, by = c("mcountry" = "name")) %>%
  filter(!is.na(geometry))  # Ensure geometries are valid

total_examples_all <- sum(year_data_all$Count, na.rm = TRUE)

# Create the first map (Overall Country-led Example)
map_all <- ggplot() +
  geom_sf(data = world_filtered, fill = "gray90", color = "white") +
  geom_sf(data = year_data_all, aes(geometry = geometry, fill = Count), color = "#00689D", alpha = 0.8) +
  scale_fill_gradient(low = "#BFDDF7", high = "#00689D") +  # EGRISS color scheme
  geom_text(data = year_data_all, aes(label = Count, geometry = geometry), stat = "sf_coordinates", size = 3, color = "black") +
  labs(title = paste("Overall Country-led Example (Total:", total_examples_all, ")")) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(), 
    axis.text = element_blank(),  # Remove degree labels
    axis.ticks = element_blank(),  # Remove axis ticks
    legend.position = "none",  # Remove legend
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank()   # Remove minor grid lines
  ) +
  coord_sf(ylim = c(-60, 80), expand = FALSE)  # Remove Arctic & Antarctica, maximize map size

# Save the first map as an image
map_all_image_path <- "map_all.png"
ggsave(map_all_image_path, map_all, width = 8, height = 6, dpi = 300)

# Step 3: Create and Save the Second Map (Overall Country-led Example Using Recommendations)

# Filter data for country-led examples where recommendations are used (PRO09 = 1)
year_data_recs <- group_roster %>%
  filter(ryear == 2024, g_conled == 1, PRO09 == 1) %>%
  group_by(mcountry) %>%
  summarise(Count = n(), .groups = "drop")

# Merge with filtered world map data
year_data_recs <- left_join(year_data_recs, world_filtered, by = c("mcountry" = "name")) %>%
  filter(!is.na(geometry))  # Ensure geometries are valid

total_examples_recs <- sum(year_data_recs$Count, na.rm = TRUE)

# Create the second map (Overall Country-led Example Using Recommendations)
map_recs <- ggplot() +
  geom_sf(data = world_filtered, fill = "gray90", color = "white") +
  geom_sf(data = year_data_recs, aes(geometry = geometry, fill = Count), color = "#4CC3C9", alpha = 0.8) +
  scale_fill_gradient(low = "#D4F0F2", high = "#4CC3C9") +  # EGRISS color scheme
  geom_text(data = year_data_recs, aes(label = Count, geometry = geometry), stat = "sf_coordinates", size = 3, color = "black") +
  labs(title = paste("Overall Country-led Example Using Recommendations (Total:", total_examples_recs, ")")) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(), 
    axis.text = element_blank(),  # Remove degree labels
    axis.ticks = element_blank(),  # Remove axis ticks
    legend.position = "none",  # Remove legend
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank()   # Remove minor grid lines
  ) +
  coord_sf(ylim = c(-60, 80), expand = FALSE)  # Remove Arctic & Antarctica, maximize map size

# Save the second map as an image
map_recs_image_path <- "map_recs.png"
ggsave(map_recs_image_path, map_recs, width = 8, height = 6, dpi = 300)

# Step 4: Combine Both Maps into a Single Image (One Below the Other)

# Load both images
map_all_img <- image_read(map_all_image_path)
map_recs_img <- image_read(map_recs_image_path)

# Combine them one below the other (stacked)
combined_maps <- image_append(c(map_all_img, map_recs_img), stack = TRUE)

# Save the final combined image
final_combined_maps_path <- "final_combined_maps.png"
image_write(combined_maps, path = final_combined_maps_path, format = "png")

# Step 5: Display the Final Combined Image in R

# Display the final combined maps in R
grid.raster(combined_maps)

# Print success message
cat("Final combined maps saved as:", final_combined_maps_path, "\n")
