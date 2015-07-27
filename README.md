# EBU-R128
EBU-R128 batch audio measurement tool

I took extra care to ensure that it is entirely stand-alone Lispworks code. None of my usual proprietary stuff, 
except in commented-out test stubs. Everything should run just fine out of the box.

The tool allows you to process one or a batch of .WAV and .AIFF files to measure Program Loudness (PL) and 
Loudness Range (LRA), along with True Peak Level (TPL). Program loudness is provided in both absolute and 
-23 dBLU relative measures. And there is an additional measure called Peak Ratio (PR) which measures the 
peak 3-sec loudness against the overall PL.

This code is not the fastest in the world, but it does run at least twice playback speed. It allows you to 
measure all the songs of an album, and find the correct normalization based on the loudest song. It is a 
non-destructive tool, merely measuring what resides in the audio files, and reports results in a plist.

Side benefits in this code include a standardized way of making CAPI prompt for files with a memory of your 
last location, as well as uncovering the secrets for WAV and AIFF formats.

Example:

(itu-rating)  ;; prompts for a file to analyze

==>  (:FILE #P"/Users/davidmcclain/projects/Music/Sine -18dBFSpk 32flt.wav" :TPL -18.0 :PL -18.0 :LU23 5.0 :LRA 0.0 :PR 0.0)

No restrictions are placed on this code.

References:  
	ITU-R BS.1770-3 Algorithms for Audio Measurements;
 	EBU-Tech 3343 Implementation of R128

- David McClain
Refined Audiometrics Laboratory, LLC
Tucson, AZ, USA
