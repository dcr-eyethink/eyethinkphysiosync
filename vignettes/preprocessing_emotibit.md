This document walks through the pre-processing stage that begins with
raw emotibit data, and ends with data that is organized, processed for
heart rate and acceleration, and imported. This is the first stage in
using the tools of the [eyethinkphysiosync
package](https://dcr-eyethink.github.io/eyethinkphysiosync/). The
functions run in R, but run a couple of external apps and bits of python
code, which is why these processes are not in the [main package
vignette](https://dcr-eyethink.github.io/eyethinkphysiosync/articles/eyethinkphysiosync_vignette.html).Firstly,
you will need the eyethinkphysiosync package of course. Here’s how to
get the latest version from github

    devtools::install_github("dcr-eyethink/eyethinkphysiosync")

Second, you need the [emotibit
software](https://github.com/EmotiBit/EmotiBit_Docs/blob/master/Getting_Started.md#Installing-EmotiBit-Software)
to parse the raw data. Thirdly, optionally, you can use
[heartpy](https://python-heart-rate-analysis-toolkit.readthedocs.io/en/latest/)
to reprocess the heart rate data, which seems to give a more relaible
result than the heart rate processor on the emotibit chip (at least, for
now). See the manual entry for emotitbit\_heartpy for [installation
instructions](https://dcr-eyethink.github.io/eyethinkphysiosync/reference/emotitbit_heartpy.html).

## Processing raw emotibit data

To harvest the data from emotbits, look on the sensor SD card for all
files created during the experiment period (the dates are encoded in
filenames). Copy all the files (.csv and .json) from the SD cards into
one data folder on computer.

### Compiling and processing emotibit data

The first step in processing the data is to place the files into folders
that are named after the sensors. This can be the serial number that
pops up on the osciloscope, or we can translate that code into a sensor
name. By default, the function uses the sensor names from the eyethink
lab, but a different .csv filename can be supplied. The .csv file should
have a column called eid (the codename you want for your sensor), and
one called serialid (the ID code from the emotibit chip). The function
will also default to duplicating the datafolder (to ‘emotibit\_data’ in
working directory), but you can set that with destination=…. If you want
to overwrite/reorganize the original folder, set destination = NULL.

    emotibit_session_folders(datafolder = "raw_emotibit",sensor_id = "eyethink",overwrite = T)

    ## [1] "emotibit_data"

Now we need to use the emotibit data parser to generate different .csv
files for each measurement. If the function can’t find your emotibit
data parser app (see above for installation instructions), it will ask
for the location. During this process, the app will pop up a window to
inform you of its progress

    emotibit_parser(datafolder = "emotibit_data")

    ## 
    ## Reading  blue8_2024-02-13_11-37-17    AX  AY  AZ  ACC calculated
    ##  blue8_2024-02-13_11-37-17  parsed
    ## Reading  green6_2024-02-13_11-32-54   AX  AY  AZ  ACC calculated
    ##  green6_2024-02-13_11-32-54  parsed
    ## Reading  red4_2024-02-13_11-27-08     AX  AY  AZ  ACC calculated
    ##  red4_2024-02-13_11-27-08  parsed

Next we can reprocess the heart rate signal using heart py and the PG
sensor data. This may not be necessary if/when the emotibit uses a
better algorithm on its chip. You’ll need to install heartpy in a
particular location on your system - see the [function
help](https://dcr-eyethink.github.io/eyethinkphysiosync/reference/emotitbit_heartpy.html)
for how to install, then tell the function where it is with
heartpy\_location=.

    emotibit_heartpy(datafolder = "emotibit_data")

    ## 
    ##  blue8_2024-02-13_11-37-17  heartpy processed
    ##  green6_2024-02-13_11-32-54  heartpy processed
    ##  red4_2024-02-13_11-27-08  heartpy processed

Finally, we need to compile all the data for the measurements that we
care about. This function goes through all the emotibit data folders and
compiles the data for the DVs that are named. It saves the data out to a
file, emotibit\_data\_comb.csv, and returns it as a datatable named ed

    processed_emotibit_data <- emotibit_compile(edv = c("HR","EA","ACC"),datafolder = "emotibit_data")

    ## 
    ## Reading  blue8_2024-02-13_11-37-17    HR  EA  ACC
    ## Reading  green6_2024-02-13_11-32-54   HR  EA  ACC
    ## Reading  red4_2024-02-13_11-27-08     HR  EA  ACC

It also returns various import checks and plots of the data. For
example, below we show a histogram of the start times for the three
sessions. Not very interesting in this example, of course

    processed_emotibit_data$p_start

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](/private/var/folders/rn/gp1l69bx3szcd4n0grjzm4b40000gp/T/RtmpRODie1/preview-cb3663b8ee9c.dir/preprocessing_emotibit_files/figure-markdown_strict/unnamed-chunk-6-1.png)

### Wrapper function

You can achieve everything above in one function call, that just runs
through the processes in turn:

    processed_emotibit_data <- emotibit_process(datafolder = "raw_emotibit")

## Analysing data

You can now link the data to behaivoural data from a log file or gorilla
experiment, and start to analyse. All these tools are detailed in the
main package vignette

    vignette("eyethinkphysiosync_vignette",package = "eyethinkphysiosync")

You can also read this vignette [online at the github
website](https://dcr-eyethink.github.io/eyethinkphysiosync/articles/eyethinkphysiosync_vignette.html).
