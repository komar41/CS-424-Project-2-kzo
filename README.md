# Don't Sleep in the Subway
This project visualizes the geographic information of all CTA L stations and explores trends and patterns in Chicago 'L' Station ridership data.

## Project Access

- **Demo URL:** [https://akashmagnadia.shinyapps.io/cs424_p2/](https://akashmagnadia.shinyapps.io/cs424_p2/)
- **Introduction video:** [https://youtu.be/VtEGv0v-xSU](https://youtu.be/VtEGv0v-xSU)
- **GitHub repo:** [https://github.com/komar41/Dont-Sleep-in-the-Subway](https://github.com/komar41/Dont-Sleep-in-the-Subway)
- **Tools used:** Python, R, and Shiny

The visualization was created for a screen with resolution of 5760x1620 (Sage screen of EVL UIC Lab).

<p>
  <img src="https://komar41.github.io/assets/img/projects/dont_sleep_in_the_subway/sage.jpeg" alt="User interface displayed in SAGE screen of EVL lab, UIC">
</p>

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
Provides details about the application, creators, publication date, and data sources.

<p>
  <img src="https://komar41.github.io/assets/img/projects/dont_sleep_in_the_subway/overview/Overview%204.png" alt="About page">
</p>

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

## Interesting Findings

**Findings 1:** As we look at the data or, the opacity of each station on the map during 2021, it seems that there were more riders on Red Line than any other line. We can also observe that O'Hare station has the most riders of all stations when we look at every line.

![image](https://github.com/user-attachments/assets/f84d2e1f-28a3-49ea-abfe-2b9ad4aa1935)

**Findings 2:** As we zoom in towards the loop, we can see that there are significantly more riders at stations in the loop and more ridership on Red Line stations near the loop. One more interesting observation we can see is that the UIC-Halsted station stands out with darker blue shade as it contains more than usual entries, which can be explained by the fact that about 85% of UIC students commute to the university.

![image](https://github.com/user-attachments/assets/5e8f4cdb-9a5a-4409-baa6-907ee8a8ea45)

**Findings 3:** Among all the Red line CTA stations, 95th/Dan Ryan was the busiest during Covid lockdown restrictions. It was also the third busiest CTA station in 2020 (source).

![image](https://github.com/user-attachments/assets/b29ac58b-f479-4a27-a695-17a41e326093)

**Findings 4:** In 2019, Lake/State had an average of 19,364 weekday passenger entries, making it the busiest 'L' station (source). During the Covid restrictions, it lost momentum in ridership. But after the restrictions were removed, it again became the busiest red line staiton.

![image](https://github.com/user-attachments/assets/a58fea2e-6bc4-4dc2-a2fe-7b2c303327eb)

**Findings 5:** Sox 35th Dan Ryan station usually has less number of riders but during games or concerts at Chicago White Sox statidum the ridership data spikes. On September 24, 2016, Chicago White Sox stadium had a record attendance of 47,754 hosting a concert of Chance the Rapper (source). Thus, we can see that the Sox 35th Dan Ryan station almost had as many entries of ridership as Ohare Airport that day.

![image](https://github.com/user-attachments/assets/0598edc3-f902-41ac-bfc3-e9636fe6a003)

**Findings 6:** As we can clearly see, most of the CTA stations have very less ridership entries on Christmas day (Dec 25th). However, O'Hare remains busy as numerous people fly to and from Chicago Airports during Christmas (source).

![image](https://github.com/user-attachments/assets/3db50374-8a9c-49c8-b4b2-39a5b6a30ceb)

**Findings 7:** On March 24, 2014 at 2:50 a.m. local time, a CTA passenger train overran the bumper at O'Hare, injuring 34 people. Following the accident, the line between O'Hare and Rosemont was closed, with a replacement bus service in place. We can clearly see from the bar chart, that due to that incident there were almost no ridership entries on OHare station on 24th March and the following week.

![image](https://github.com/user-attachments/assets/8ad14157-bd68-494c-8549-f9c6717dd182)

**Findings 8:** The Grant Park Music Festival is a ten-week (Jul 2 – Aug 21, 2021) classical music concert series held annually in Chicago, Illinois, United States. During this time Washington/Wabash station remains busy as it is the public transport access to Grant Park. Also, the station remains busy as Millennium Park is a very popular destination for visitors.

![image](https://github.com/user-attachments/assets/33d0d0c0-ce8f-4d1f-89c3-89c4e7ad25c9)

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
