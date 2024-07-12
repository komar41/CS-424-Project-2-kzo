# Don't Sleep in the Subway

## Project Access

- **Demo URL:** [https://akashmagnadia.shinyapps.io/cs424_p2/](https://akashmagnadia.shinyapps.io/cs424_p2/)
- **Introduction video:** [https://youtu.be/VtEGv0v-xSU](https://youtu.be/VtEGv0v-xSU)
- **GitHub repo:** [https://github.com/komar41/Dont-Sleep-in-the-Subway](https://github.com/komar41/Dont-Sleep-in-the-Subway)
- **Tools used:** Python, R, and Shiny

The visualization was created for a screen with resolution of 5760x1620 (Sage screen of EVL UIC Lab).

<p>
  <img src="https://komar41.github.io/assets/img/projects/dont_sleep_in_the_subway/sage.jpeg" alt="User interface displayed in SAGE screen of EVL lab, UIC">
</p>

## Introduction

This project visualizes the geographic information of all CTA L stations and explores trends and patterns in Chicago 'L' Station ridership data.

## User Interface

### All Stations Page

Features:
- Displays total ridership entry for a single date across all CTA stations
- Station opacity on the map changes according to ridership entry
- Date selection option
- Station highlighting on map and bar chart
- Line color selection
- Sortable bar chart and data table
- Navigation buttons for date selection

<p>
  <img src="https://komar41.github.io/assets/img/projects/dont_sleep_in_the_subway/overview/Overview%201.png" alt="All stations page">
</p>

#### Ridership Difference Between Two Dates

- Checkbox to enable comparison between two dates
- Divergent color scheme on bar chart to show changes

<p>
  <img src="https://komar41.github.io/assets/img/projects/dont_sleep_in_the_subway/overview/Overview%202.png" alt="All stations page - ridership difference between two dates">
</p>


### One Station Page

Features:
- Overview of ridership data for a particular CTA station
- Station selection via map or dropdown menu
- Line color filtering
- Yearly bar chart
- Daily, monthly, or weekday chart options
- Year selection for detailed chart
- Raw data display

<p>
  <img src="https://komar41.github.io/assets/img/projects/dont_sleep_in_the_subway/overview/Overview%203.png" alt="One station page">
</p>

### About Page

<p>
  <img src="https://komar41.github.io/assets/img/projects/dont_sleep_in_the_subway/overview/Overview%204.png" alt="About page">
</p>

Provides details about the application, creators, publication date, and data sources.

## About the Data

Data sources from Chicago Data Portal:

1. [CTA L Stops Information](https://data.cityofchicago.org/Transportation/CTA-System-Information-List-of-L-Stops/8pix-ypme) (48KB)
2. [CTA Ridership Data](https://data.cityofchicago.org/Transportation/CTA-Ridership-L-Station-Entries-Daily-Totals/5neh-572f) (39MB)

### CTA L Stops Information

Includes:
- Location and service availability information
- Station names, stop descriptions, and line colors
- Direction IDs and unique identifiers
- ADA compliance information

<p>
  <img src="https://komar41.github.io/assets/img/projects/dont_sleep_in_the_subway/data/Data%201.png" alt="CTA L stops information data">
</p>

### CTA L Station Ridership Data

Contains:
- Daily ride entries for all CTA stations (2001-2021)
- Combined entries for all turnstiles per station
- Day types: W (Weekday), A (Saturday), U (Sunday/Holiday)

<p>
  <img src="https://komar41.github.io/assets/img/projects/dont_sleep_in_the_subway/data/Data%202.png" alt="CTA L station ridership data">
</p>

### Data Processing

Due to Shiny server limitations, the ridership data was split into smaller files. Python script used for splitting:

```python
#!/usr/bin/env python3

import csv
import os
import sys

os_path = os.path
csv_writer = csv.writer
sys_exit = sys.exit

if __name__ == '__main__':
   # number of rows per file
   chunk_size = 130000

   # file path to master tsv file
   file_path = "C:/Users/Akash/UIC/CS 424/tsv_splitter/CTA_-_Ridership_-__L__Station_Entries_-_Daily_Totals.tsv"

   if (not os_path.isfile(file_path) or 
        not file_path.endswith('.tsv')):
       print('You must input path to .tsv file for splitting.')
       sys_exit()
   file_name = os_path.splitext(file_path)[0]

   with open(file_path, 'r', newline='', encoding='utf-8') as tsv_file:
       chunk_file = None
       writer = None
       counter = 1
       reader = csv.reader(tsv_file, delimiter='\t', quotechar='\'')

       # get header_chunk
       header_chunk = None
       for index, chunk in enumerate(reader):
           header_chunk = chunk
           header_chunk[0] = header_chunk[0][1:]
           break

       for index, chunk in enumerate(reader):
           if index % chunk_size == 0:
               if chunk_file is not None:
                   chunk_file.close()

               chunk_name = '{0}_{1}.tsv'.format(file_name, counter)
               chunk_file = open(chunk_name, 'w', newline='', encoding='utf-8')
               counter += 1
               writer = csv_writer(chunk_file, delimiter='\t', quotechar='\'')
               writer.writerow(header_chunk)
               print('File "{}" complete.'.format(chunk_name))

           chunk[1] = chunk[1].replace("'", "")
           writer.writerow(chunk)




















Application Link: https://akashmagnadia.shinyapps.io/cs424_p2/

Detailed Documentation: https://sites.google.com/view/cs424-komar3/project-2

The visulization was created for a screen with resolution of 5760x1620 (Sage screen of EVL UIC Lab). [For reference see picture below]

This project was built for visualizing the trends and interesting patterns in all Chicago 'L' Station ridership data over the years (2001-2021).

![IMG_1273](https://user-images.githubusercontent.com/90569118/168742352-4cb317f3-dbf4-456e-b3c7-d1a05a7c861c.jpg)
