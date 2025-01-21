##This code is for extracting representative video snippet and highlight videos from video data annotated using EventMeasure software.
##Code prepared by jacquomo.monk@utas.edu.au and justin.hulls@utas.edu.au

library(tidyverse)
library(readxl)
library(lubridate)


setwd('R:\\TAFI\\Data\\CERF\\BOSS Data\\202402_Flinders')

#read file with the drop times
clip_time <-read_xlsx("GIS/BOSS_Sites_with_Depth_All.xlsx")

# convert the date and DateTime to a date type
clip_time <- clip_time %>%
  mutate(
    Date = as.Date(Date),
    DateTime = ymd_hms(DateTime)
    )%>%
    rename(video_date = Date)


# Calculate cumulative seconds within each date group
clip_time <- clip_time %>%
  group_by(video_date) %>%
  mutate(
    time_in_seconds = as.numeric(difftime(DateTime, min(DateTime), units = "secs"))
  ) %>%
  ungroup()

#read in the video time to the first drop table 
vidcomp_time_to_drop <- read_xlsx("CompVideo_Time_to_Drop_OctNov24.xlsx")

# Convert time to sec
vidcomp_time_to_drop <- vidcomp_time_to_drop %>%
  mutate(
    video_time_to_1st_drop = as.numeric(hms(substring(video_time_to_1st_drop,12)))
  ) %>% 
  rename(video_date = Date)

# # Merge clip_time and vidcomp_time_to_drop by 'Date' and Add the time to first drop from vidcomp_time_to_drop, plus 60 seconds

clip_time <- clip_time %>%
  # Perform an inner join to keep only matching dates
  left_join(vidcomp_time_to_drop, by = "video_date") %>%
  group_by(video_date) %>%
  arrange(DateTime) %>%  # Ensure drops are ordered within each date
  mutate(
    # Replace NA in video_time_to_1st_drop with 0
    video_time_to_1st_drop = replace_na(video_time_to_1st_drop, 0),
    video_clip_time = if_else(
      row_number() == 1,  # For the first drop of the day
      time_in_seconds + video_time_to_1st_drop + 60,  # Add video_time_to_1st_drop and 60 seconds
      first(time_in_seconds + video_time_to_1st_drop + 60) + cumsum(c(0, diff(time_in_seconds)))  # Cumulative offset
    )
  ) %>%
  ungroup()


# Define the directory containing your videos
video_dir <- "R:/TAFI/Data/CERF/BOSS Data/202402_Flinders/CompVid_20241026-2"

# List all MP4 files in the directory
video_files <- list.files(video_dir, pattern = "\\.mp4$", full.names = TRUE)

# Function to get video duration using ffprobe
get_video_duration <- function(video_path) {
  command <- paste("ffprobe -i", shQuote(video_path), 
                   "-show_entries format=duration -v quiet -of csv=\"p=0\"")
  duration <- system(command, intern = TRUE)  # Run the command and capture the output
  return(as.numeric(duration))  # Convert the output to numeric
}

# Create a data frame of video files and their durations
video_durations <- data.frame(
  video_name = basename(video_files),
  video_duration = sapply(video_files, get_video_duration)
)

# Extract the date (yyyymmdd), video_number and video_location from the video name
video_durations <- video_durations %>%
  mutate(
    video_date = str_extract(video_name, "\\d{8}"),   # Finds 8 consecutive digits
    video_date = as.Date(video_date, format = "%Y%m%d"),  # Converts to date
    video_number = as.numeric(str_sub(video_name, -8,-5)),
    video_location = str_extract(video_name,"_[BT]"),
    video_location = str_remove(video_location,"_")
  )

# caculate the cumulative run time of each video.
video_durations <- video_durations %>%
  arrange(video_date, video_number, video_location) %>%
  group_by(video_date,video_location) %>%
  mutate(
    cumulative_time_in_seconds = cumsum(video_duration)
  ) %>%
  ungroup()



# Filter for "B" videos and sort the data
filtered_video_durations <- video_durations %>%
  filter(video_location == "T") %>%
  arrange(video_date, video_number)

#filtered clip_time for na
filtered_clip_time <- clip_time %>% 
  filter(!is.na(Video)) %>% 
  select(video_date,Site,video_clip_time,DateTime) %>% 
  arrange(video_date, video_clip_time)
  
  
# Ensure both datasets have consistent date formats
filtered_clip_time <- filtered_clip_time %>%
  mutate(video_date = as.Date(video_date))

filtered_video_durations <- filtered_video_durations %>%
  mutate(video_date = as.Date(video_date))

# Initialize an empty data frame to store results
all_results <- data.frame()

# Process each unique date in the clip_time dataset
for (date in unique(filtered_clip_time$video_date)) {
  
  # Filter clip_time and video_durations for the current date
  date_clip_time <- filtered_clip_time %>% filter(video_date == date)
  filtered_video_durations_for_date <- filtered_video_durations %>%
    filter(video_date == date, !is.na(cumulative_time_in_seconds)) %>%
    arrange(cumulative_time_in_seconds)  # Ensure sorted cumulative time
  
  # Initialize variables to track previous video cumulative time
  previous_video_end_time <- 0
  
  # Empty vector to store video names and clip offsets
  video_for_clip <- character(length = nrow(date_clip_time))
  clip_offset_within_video <- numeric(length = nrow(date_clip_time))
  
  # Iterate over each clip time and calculate the correct video and offset
  for (i in 1:nrow(date_clip_time)) {
    video_clip_time <- date_clip_time$video_clip_time[i]
    
    # Find the correct video by checking the cumulative times
    video_index <- which(filtered_video_durations_for_date$cumulative_time_in_seconds >= video_clip_time)
    selected_video <- filtered_video_durations_for_date$video_name[video_index[1]]
    
    # Calculate the clip offset within the selected video
    if (video_index[1] == 1) {
      clip_offset_within_video[i] <- video_clip_time  # First video, no need to subtract
    } else {
      clip_offset_within_video[i] <- video_clip_time - filtered_video_durations_for_date$cumulative_time_in_seconds[video_index[1] - 1]
    }
    
    # Store the selected video for the current clip
    video_for_clip[i] <- selected_video
  }
  
  # Combine the results into the data frame
  merged_data <- data.frame(
    Site = date_clip_time$Site,
    video_clip_time = date_clip_time$video_clip_time,
    video_for_clip = video_for_clip,
    clip_offset_within_video = clip_offset_within_video
  )
  
  # Append the result for this date to the overall results
  all_results <- bind_rows(all_results, merged_data)
}



##for truble shooting times
all_results_min <- all_results %>%
  mutate(
    # Convert video_clip_time from seconds to mins:secs format
    time_in_min_sec = sprintf("%d:%02d", floor(video_clip_time / 60), round(video_clip_time %% 60)),
    
    # Ensure clip_offset_within_video is numeric and convert it from seconds to mins:secs format
    clip_offset_within_video = as.numeric(clip_offset_within_video),  # Ensure it's numeric
    video_clip_min_sec = sprintf("%d:%02d", floor(clip_offset_within_video / 60), round(clip_offset_within_video %% 60))
  )




# Specify the out put directory fo the frame grabs
output_dir <- "R:/TAFI/Data/CERF/BOSS Data/202402_Flinders/CompVid_20241026-2/img_grab"

# Ensure the output directory exists
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Generate and execute FFmpeg commands directly from R

all_results %>%
  mutate(
    # Build the full path for the input video file, properly quoted
    video_full_path = paste0("\"", file.path(video_dir, video_for_clip), "\""),
    # Build the full path for the output frame file, properly quoted
    output_frame_path = paste0("\"", file.path(output_dir, paste0("FI_", Site, "_top.jpg")), "\""),
    # Create the FFmpeg command
    ffmpeg_command = paste0(
      "ffmpeg -i ", video_full_path, 
      " -ss ", clip_offset_within_video, 
      " -frames:v 1 ", output_frame_path
    )
  ) %>%
  pull(ffmpeg_command) %>%
  walk(~ {
    cat("Running: ", .x, "\n")
    system(.x)  # Execute FFmpeg command
  })





########################## Used to Take only update frames grabs for particular sites, Saves have to redo the whole lot ##################################  
# Ensure the output directory exists
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Filter for video_date of 2024-10-26 and generate/execute FFmpeg commands
all_results_date %>%
  filter(Site %in% c(9, 103)) %>%  # Restrict to the specific date
  mutate(
    # Build the full path for the input video file, properly quoted
    video_full_path = paste0("\"", file.path(video_dir, video_for_clip), "\""),
    # Build the full path for the output frame file, properly quoted
    output_frame_path = paste0("\"", file.path(output_dir, paste0(Site, ".jpg")), "\""),
    # Create the FFmpeg command
    ffmpeg_command = paste0(
      "ffmpeg -i ", video_full_path, 
      " -ss ", clip_offset_within_video, 
      " -frames:v 1 ", output_frame_path
    )
  ) %>%
  pull(ffmpeg_command) %>%
  walk(~ {
    cat("Running: ", .x, "\n")
    system(.x)  # Execute FFmpeg command
  })


