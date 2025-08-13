# --- Step 1: Load the magick library ---
library(magick)

# --- Step 2: Define our robust image processing function ---

# This function will take any image, trim its whitespace, scale it to fit
# inside the target dimensions, and then paste it onto a centered canvas.
process_image <- function(image, target_width = 800, target_height = 800) {
  
  # 1. Trim all surrounding whitespace from the image
  trimmed_image <- image_trim(image)
  
  # 2. Scale the trimmed image to fit within the target dimensions
  scaled_image <- image_scale(trimmed_image, paste0(target_width, "x", target_height))
  
  # 3. Create a new, blank canvas of the exact target size
  canvas <- image_blank(width = target_width, height = target_height, color = "white")
  
  # 4. Composite (paste) the scaled image onto the center of the canvas
  final_image <- image_composite(canvas, scaled_image, gravity = "center")
  
  return(final_image)
}


# --- Step 3: Read and Process all four images using our function ---
image1 <- image_read("Penguins.png")
image2 <- image_read("fish.png")
photo1 <- image_read("canal.jpg")
photo2 <- image_read("turnstones.jpg")

# Apply our robust function to each image
image1_processed <- process_image(image1)
image2_processed <- process_image(image2)
photo1_processed <- process_image(photo1)
photo2_processed <- process_image(photo2)


# --- Step 4: Assemble the grid (this logic is the same) ---

# Combine the first two images to create the top row
top_row <- image_append(c(image1_processed, image2_processed))

# Combine the next two images to create the bottom row
bottom_row <- image_append(c(photo1_processed, photo2_processed))

# Stack the rows vertically to create the final 2x2 grid
final_cover <- image_append(c(top_row, bottom_row), stack = TRUE)


# --- Step 5: Save the final composite image ---
image_write(final_cover, path = "my-cover.png", format = "png")

# Optional: Display the final image to verify it worked
print(final_cover)