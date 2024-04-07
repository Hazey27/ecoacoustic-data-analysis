#Lake George sound recording analysis


#IMPORTING
library("tidyverse")
library("tuneR")
library("sound")
library("seewave")

#pam guide
#underwater soundscape monitoring

#--------------------------------------------------------------------------------------------
#LOADING DATA

#Stores all .wav file names in Soundtrap_Wreck_2021.08.10 in file_names
file_names <- list.files(path = "~/Lake_george_sound_analysis/wav_files", 
                         pattern = "*.wav", all.files = FALSE, full.names = FALSE, recursive = FALSE, ignore.case = FALSE, 
                         include.dirs = FALSE, no.. = FALSE)

#Sets working directory to Soundtrap_Wreck_2021.08.10
setwd("~/Lake_george_sound_analysis/wav_files")

metadata <- data.frame(file_names)

dttm_regex <- "\\d+\\.(\\d+)\\.wav"
dttm <- str_match(file_names, dttm_regex)
metadata$dttm <- ymd_hms(dttm[ , 2], tz = "EST")
metadata$wav <- 0

# Read each .wav file and store Wave objects in a list
wav_list <- lapply(file_names, readWave)


# Add the list of Wave objects to the metadata data frame
#metadata$wav <- wav_list


# wav_table <- data.frame(cbind(sound.files = wav_list, selec = 1, start = 0, end = 1800))

# wav_table$selec <- as.integer(wav_table$selec)
# wav_table$start <- as.integer(wav_table$start)
# wav_table$end <- as.integer(wav_table$end)

# selection_table(wav_table)

#--------------------------------------------------------------------------------------------

#here's where'd I use tuneR functions to visualize and analyze the data
#look at relative sound levels, frequencies, etc


# Lets test stuff using wav_list[7]

# test_wav <- wav_list[7]

#test_quant <- quantize(test_wav, parts = 1)






# using sound package
# 
# wav_sample <- loadSample(metadata[1, 1])
# samp_sound <- sound(wav_sample)
# samp_freq <- rate(wav_sample)
# 
# # creating time representation of sound
# samp_array <- ((0:(86401232-1)) / samp_freq) * 1000 # 86401232 from dim(samp_sound)[2]
# 
# #plots tone DOESNT WORK; FREEZES R
# #plot(samp_array, samp_sound, col = "black", xlab = "Time (ms)", ylab = "Amplitude") #doesn't seem to work
# 
# wav_acoustat <- acoustat(wav_list[[12]], f = 48000, plot = TRUE) #12 is recording from 10 pm
# 
# # Plotting acoustat data (amp and freq)
# ggplot(data.frame(wav_acoustat$time.contour), aes(x = time, y = contour)) +
#   geom_point()
# 
# ggplot(data.frame(wav_acoustat$freq.contour), aes(x = frequency, y = contour)) +
#   geom_point()
# 
# amp_data <- data.frame(wav_acoustat$time.contour)
# 
# #find peak crop +-3 sec
# 
# #find peaks:
# #peaks <- dplyr::filter(amp_data, contour < 5e-4, contour >= 1e-4) # 687 values fit these requirements, though many are from same/similar time frame
# peaks <- dplyr::filter(peaks, contour < 5e-4, contour >=1.2e-4) # 180 rows, much more reasonable
# # DOES NOT LINE UP WITH INC VOLUME (audacity amplify)







#-----------------------------------------------------------------------------------------------------------------------

wave <- wav_list[[12]]

# Extract time and amplitude information
time <- (1:length(wave@left)) / as.double(wave@samp.rate) # the @ looks weird but is correct
#can't only look at seconds or milliseconds from the get go bc different amplitude value for each
amplitude <- wave@left

# Create a data frame with time and amplitude
data <- data.frame(time_s = time, amplitude = amplitude) # time divided by samp.rate so in seconds

# graph_start_time <- Sys.time()
# ggplot(data, aes(x = time, y = amplitude)) + #never prints
#   geom_line() +
#   ylim(c(-50, 50))
# graph_end_time <- Sys.time()

midpoint <- mean(data$amplitude)

# Pos peaks peaks
pos_peaks <- dplyr::filter(data, amplitude < 500, amplitude >= midpoint + 40, time >= 5)
# 62,012 obs

neg_peaks <- dplyr::filter(data, amplitude > -500, amplitude <= midpoint - 40, time >=5)
# 323,328 obs

peaks <- rbind(pos_peaks, neg_peaks)


#selects for whole number seconds to reduce amount of data to look through (this misses a lot, only returns 2 for pos_peaks)

identify_poi <- function(vector) { # poi = points of interest
  poi = 0
  j = 0
  for(i in 2:length(vector)) {
    if(vector[i] - vector[i-1] > 0.005) { # only adds values that are greater than  0.5 seconds away from the last value. hopefully this doesn't drop any points of interest, but unless the peak is much greater than 0.5 s, it shouldn't
      j = j + 1
      poi[j] = i
    }
  }
  return(poi)
}

time_poi = identify_poi(peaks$time)
poi = data.frame(cbind(time_s = peaks$time_s[time_poi], amplitude = peaks$amplitude[time_poi]))


print(paste(length(time) - length(time_poi), "timepoints were eliminated.", length(time_poi), "remain.")) # so we shrank the POIs, but there's still too many


# ggplot(poi, aes(x = time, y = amplitude)) +
#   geom_point() + 
#   geom_hline(yintercept = midpoint)


# TYPES OF ANALYSIS RECOMMENDED BY LINDSETH & lOBEL PAPER (jk no longer doing that)

# Power spectral density -- shows which frequencies vary the most? --------------------------------------
# In R, the PSD of a time series data set can be calculated by dividing the results of fft() by the frequency bin width.
# when using FFT(), the bin width can be calculated by dividing the sample rate by the FFT length; or by dividing the bandwidth by the number of bins (which is equal to 1/2 the FFT length)

# ALL FFT CODE IN JAIL
# using tiny bit of data to test it out
# small_indices = sample(1:86396848, round(0.01 * 86396848))  # 0.01% of data in wave 12; hard coded numbers bc printing length took like 30 sec
# 
# small_data = data[small_indices, ]
# 
# fft_start = Sys.time()
# fft_wave_small = fft(small_data$amplitude) #lets see how long this takes...
# fft_end = Sys.time()
# 
# # fft complex number to represent amplitude/magnitude and phase/frequency
# # want to split these up into their own columns
# fft_wave_small_amp = lapply(fft_wave_small, FUN = Re)
# fft_wave_small_freq = lapply(fft_wave_small, FUN = Im)
# 
# fft_wave_small_df_loop = list()
# for(i in 1:length(fft_wave_small)) {
#   fft_wave_small_df_loop$amp[i] = Re(fft_wave_small[i])
#   fft_wave_small_df_loop$freq[i] = Im(fft_wave_small[i])
# }

# fft_wave_small_df = data.frame(amp = fft_wave_small_amp, freq = fft_wave_small_freq)
# Gives this error: Error: cannot allocate vector of size 2.0 Gb

# took like 22 GB memory and graph never loaded so this code goes in jail
# df_start = Sys.time()
# fft_wave_12_df = data.frame(cbind(fft_wave_12_amp, fft_wave_12_freq)) # I think this just did rbind and/or c() and idk why :sob:
# df_end = Sys.time()
# 
# 
# fft_graph_start = Sys.time()
# ggplot(data.frame(fft_wave_small_df_loop), aes(x = freq, y = amp)) +
#   geom_point() +
#   ylim(c(5E5, -5E5)) +
#   xlab("Frequency") +
#   ylab("Amplitude") +
#   ggtitle("FFT for random 1% of wave 12 data") +
#   labs(subtitle = "Can't graph all of the data because it's 11.1 GB")
# fft_graph_end = Sys.time()
# 
# # gonna take a sample and use that to test for now
# fft_sample_indices = sample(1:(length(fft_wave_12_df)/2), size = (length(fft_wave_12_freq)/2) * 0.01)
# fft_sample = fft_wave_12_df[fft_sample_indices, ]
# 
# 
# fft_graph_start = Sys.time()
# ggplot(data.frame(fft_sample), aes(x = fft_wave_12_freq, y = fft_wave_12_amp)) +
#   geom_point()
# fft_graph_end = Sys.time()




# Spectral analysis ------------------------------------------------------------------------------


trimmed_wave = cutw(wave, from = 20, to = 1789.535, plot = FALSE, output = "Wave") # vals in seconds

spectrogram = spectro(trimmed_wave, f = 44100, fastdisp = FALSE, norm = TRUE, cont = FALSE, flog = FALSE, flim = c(0, 0.25))


trimmed_wav_list = lapply(wav_list, FUN = cutw, wave = wave, from = 20, to = 1789.535, plot = FALSE, output = "Wave")
