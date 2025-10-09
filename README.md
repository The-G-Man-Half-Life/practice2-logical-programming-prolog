# Practice Number 2 - Logical Programming in ProLog

This project contains 2 distinct modules both having distinct knowledge databases and different queries that try to answer some common petitions that an user could have related to the information contained inside the knowledge databases being the first focused on computing platforms and the second one focused on trips and travels.

## Contents
- `challenge1_database`
  - `src`
    - `complexQueries`
      - `bestCPAfter2023.pl`
      - `mostCommCPUGPUManComb.pl`
      - `mostCommRamVRamInGBComb.pl`
      - `worstCPBefore2023.pl`
    - `queries`
      - `amdAfter2021.pl`
      - `amountOfCPASUS.pl`
      - `cpHDBetween.pl`
      - `laptopsRamHD.pl`
      - `tablet2PlusGBRam.pl`
    - `facts.pl`
  - `main.pl`
 - `challenge2_TPS`
    - `filters_Optimals.pl`
    - `main.pl`
    - `outputs.pl`
    - `route_facts.pl`
    - `routes_logic.pl`
  - `video`
    - `link.txt`
  - `README.md`

## Program Functionalities

### 1) challenge1_database

### complexQueries

- **bestCPAfter2023.pl**  
  Takes care of finding the best computing platform after 2023 from facts.pl
  implementing a punctuation system and a recursive function that organizez
  them highest to lowest punctuation and then takes the element in the top
  which is according to the punctuation system the best computing platform.

- **mostCommCPUGPUManComb.pl**  
  Takes care of finding the most common CPU/GPU manufacturer combinations
  for each trademark using the data from facts.pl. This is achieved by
  only taking into account the trademark, the CPU manufacturer and the
  GPU manufacturer. Then by using a recursive a function it counts the
  amount of apparitions of every CPU and GPU manufacturer combination
  to finally in another function organize it from highest to lowest
  count so It can finally choose the one in the Head of the list as it
  is the one with the highest count

- **mostCommRamVRamInGBComb.pl**  
  Takes care of finding the most common Ram/VRam in GB combination using the
  data from facts.pl. It does it by gathering only The Ram and VRam, in GB,
  from each computing platform from facts.pl and then using these combinations
  to count the amount of apparitions of every unique Ram/VRam combination to
  finally sort it from highest to lowest count and finally taking the one which
  is at the top of the sort as it is the one with the biggest amount of counts.

- **worstCPBefore2023.pl**  
  Takes care of finding the worst computing platform before 2023 from facts.pl.
  It does it by implementing a punctuation system which gives a certain value
  to each accountable characteristic from the computing platforms and then by
  by using a function that sort from lowest to highest we take the first item
  which is the worst according to the punctuation system given.

### queries

- **amdAfter2021.pl**  
  Takes care of finding all the computing platforms with AMD CPU after 2021
  from facts.pl.This is achieved by only choosing those computing platforms
  that go according to the conditions given then all of them are gathered
  in a list and finally printed all this using recursion.

- **amountOfCPASUS.pl**  
  Takes care of counting the amount of computing platforms from ASUS and also
  displaying all of the ASUS computing platforms from facts.pl. This is
  achieved by only choosing those computing platforms that go according
  to the conditions given then all of them are gathered in a list and then
  counted using a recursive function and with the list of computing platforms
  and the count gathered all these informations are printed.

- **cpHDBetween.pl**  
  Takes care of finding all the computing platforms with hard disk capacities
  between 32 and 512 GB from facts.pl. This is achieved by only choosing those
  computing platforms that go according to the conditions given then all of
  them are gathered in a list and and with the list of computing platforms
  all these informations are printed.

- **laptopsRamHD.pl**  
  Takes care of finding all the computing platforms where the type of computing
  platform is a laptop, it has more than 4 GB of RAM and less than 512 GB of
  hard disk from facts.pl.This is achieved by only choosing those computing
  platforms that go according to the conditions given then all of them are
  gathered in a list and and with the list of computing platforms all these
  informations are printed.

- **tablet2PlusGBRam.pl**  
  Takes care of finding all the tablets with more than 2 GB of RAM from facts.pl.
  This is achieved by only choosing those computing platforms that go according
  to the conditions given then all of them are gathered in a list and and with
  the list of computing platforms all these informations are printed.

## Problems and errors during the development

### complexQueries

**bestCPAfter2023.pl**  
- Determining how to properly measure which devices are better than the
  other ones.
- Understanding what score to give to each characteristic based on its
  importance for the device.
- Making the logic behind the recursive method to choose the computing
  platform with the highest punctuation.
- Joining all of the queries to make the main query work.
- Deciding how to display all of the gathered information so it can be
  visually attractive

**mostCommCPUGPUManComb.pl**  
- Finding out how to make the logic for finding the most common CPU
  manufacturer and GPU manufacturer for each trademark as this logic
  was kind of complex having in mind the usage of recursion.
- Designing the countOcurrences method so it could return the amount
  of counts for each CPU/GPU manufacturer combination and also be the
  backbone of the main query.
- Finding out how to join all of the queries in the main one to make
  everything work.
 **mostCommRamVRamInGBComb.pl**  
- Trying to make all of the functions generally usable not matter the
  situation so these functions could be re used or reimplemented for
  all the future queries.
- Figuring out how to print all the information in a visually attractive
  form as this query was pretty distinct compared to the other queries
  made before in the way of printing the information. 

- **worstCPBefore2023.pl**  
  -Finding out how to re implement some old queries to make this new query
   in the most optimal way possible.
  -Understanding how to stablish dates and times properly to obtain the
   correct facts and data. 

### queries

**amdAfter2021.pl**  
  - Discovering the appropiate ways to print information implementing
    recursion.

**amountOfCPASUS.pl**  
  - Naming the file. as the Brand is named ASUS and not Asus but it
    could be misunderstood so it was decided to leave it in its normal
    format as it is generally understood as the trademark.
  - Making the function to count the amount of items that were gathered
    during the proccess as this one had a different proccess of recursion.

**cpHDBetween.pl**  
  - Naming the file briefly and understandable as the goal of this
    file is quite large to explain in a short file name format.

**laptopsRamHD.pl**  
  - Making the most visually appealing the header of the print list function
    becase the original names were too large for each category so it was
    necessary to find some short names.

- **tablet2PlusGBRam.pl**  
  - Having to change the knowledge database as there were not enough tablets
    to make this query visibly good enough.

      
### General problems throught the development

   - Installing swish-Prolog and all of the nece-
     ssary components to make it work because
     Microsoft defender evited the installation of
     certain files that were obligatory for the
     proper function of Prolog.
   - Understanding how to use Prolog in VSC as
     Prolog has its own console to execute the
     queries but we wanted to make it run on VSC's
     console for proper testing and more
     compatibility with Github and also due to
     the extensions that VSC provides.
   - Coordinating all the proccess through Github.
     Since we divided the work between all members
     coordinating how to use the tool appropiately
     to evite fatal mistakes was really important
     and also to inform the proper structures for
     the documentation, the commentaries the kind
     of typing for the code and the folder structure.
   - Time organization and video preparation.
   - Understanding how to use the modules to import
     and export codes between files so in that way the
     code could be more readable and better to maintain
     along the whole process
   - Understanding better the recursion as the prolog
     kind of recursion is quite much different to the
     one of haskell or an average POO one
   - Understanding the new types of variables for the
     facts since prolog includes the basic types but
     also some new ones as Atoms but with different types
     of atoms between each other.
   - Understanding some new methods in Prolog such as
     setof(), sort(), member() and findall() as these
     methods had their unique ways of working and were
     necessary for making the queries.
     
## Conclusions

  - The development of this project taught us the im-
    portance of learning and adapting to new languages
    to sort all kind of problems and difficulties
    specially when this brings a new understanding of
    different kinds of programming paradigms.
  - It is really important to understand how to sepa
    rate properly all the functionalities and programs
    inside the project to maintain a good cohesion and
    order along the whole development proccess,
    making it better to read and change if is necessary.
  - It is necessary to understand the use of tools
    such as Github and VSC to collaborate with other
    people to bring to life all kind of different projects
    one could ever have.
  - Understanding Recursion can be pretty tedious and
    annoying at first glance but learning it can open
    a world of possibilities for iteration problems.
  - Learning how to install new languages can be seen
    as a challenge, specially prolog and its problem
    with Microsoft Defender, but is really important
    to deal with this kind of challenges for the future 
  - The teamwork was really important in this project
    as We helped each other and learned tons of things
    of each other.
  - Good expression and communication was vital to make a
    good video.
```bash
## tests 
Output of `amdAfter2021PrintList/0`
--------------------------------------------
Trademark | Name | Serial number | Year of acquisition
--------------------------------------------
('HP'|'Omen 16'|'5CDV9D4N'|2023)
--------------------------------------------
('HP'|'Omen 15'|'5CDU1E5P'|2022)
--------------------------------------------
('Lenovo'|'LOQ 15 Gen 9'|'LNVW6D4N'|2023)
--------------------------------------------
('Lenovo'|'IdeaPad Gaming 3'|'LNVU1E5P'|2022)
--------------------------------------------
('ASUS'|'ROG Zephyrus G16'|'ASY8B2L'|2025)
--------------------------------------------
('ASUS'|'ROG Flow X13'|'ASZ7C3M'|2024)
--------------------------------------------
('ASUS'|'TUF Gaming A15'|'AST6D4N'|2023)
--------------------------------------------
('ASUS'|'ROG Flow Z13'|'ASTAB01'|2025)
--------------------------------------------
('MSI'|'Crosshair 17 HX'|'MSIU1E5P'|2024)
--------------------------------------------
('Razer'|'Blade 16 OLED'|'RZRY8B2L'|2025)
--------------------------------------------
('Razer'|'Blade 14 Ryzen'|'RZRW6D4N'|2023)
--------------------------------------------
('Dell'|'Alienware m15 R7'|'DLT3F6Q'|2023)
--------------------------------------------
('Razer'|'Tomahawk ATX'|'RZRDC02'|2024)
--------------------------------------------

Output of `amountOfCPASUSPrintList/0`
--------------------------------------------
This is the amount of ASUS computing platforms: 7
--------------------------------------------
These are all of the  ASUS computing platforms: 
--------------------------------------------
Name | Serial number | Year of acquisition | Type of Computing platform
--------------------------------------------
('ROG Strix Scar 18'|'ASX9A1Q'|2025|laptop)
--------------------------------------------
('ROG Zephyrus G16'|'ASY8B2L'|2025|laptop)
--------------------------------------------
('ROG Flow X13'|'ASZ7C3M'|2024|laptop)
--------------------------------------------
('TUF Gaming A15'|'AST6D4N'|2023|laptop)
--------------------------------------------
('ROG Strix G15'|'ASU1E5P'|2022|laptop)
--------------------------------------------
('ROG Flow Z13'|'ASTAB01'|2025|tablet)
--------------------------------------------
('ROG Flow Z13'|'ASTAB02'|2023|tablet)
--------------------------------------------

Output of `cpHDBetweenPrintList/0`
--------------------------------------------
Trademark | Name | Serial number | Hard disk capacity in GB
--------------------------------------------
('Lenovo'|'Legion Tab Gen 3'|'LNTG8A1X'|256)
--------------------------------------------

Output of `laptopsRamHDPrintList/0`
--------------------------------------------
This is the amount of Computing Platforms with
more than 4 GB of Ram and less than 512 GB of 
Hard Disk: 1
--------------------------------------------
These are all of the found computing platforms: 
--------------------------------------------
Name | Serial number | Ram capacity in GB | Hard disk capacity in GB
--------------------------------------------
('Legion Tab Gen 3'|'LNTG8A1X'|12|256)
--------------------------------------------

Output of `tablet2PlusGBRamPrintList/0`
--------------------------------------------
Trademark | Name | Serial number | Ram capacity in GB
--------------------------------------------
('Lenovo'|'Legion Tab Gen 3'|'LNTG8A1X'|12)
--------------------------------------------
('ASUS'|'ROG Flow Z13'|'ASTAB01'|128)
--------------------------------------------
('ASUS'|'ROG Flow Z13'|'ASTAB02'|32)
--------------------------------------------
('RedMagic'|'Astra Gaming Tablet'|'RDMTAB01'|24)
--------------------------------------------
('RedMagic'|'NOVA Gaming Tablet'|'RDMTAB02'|16)
--------------------------------------------

Output of `bestComputingPlatformPrint/0`
--------------------------------------------
The most powerful computing platform after 2023 is the: 
Legion 9i Gen 10
TradeMark: Lenovo
Serial number: LNVX9A1Q
Ram Capacity in GB: 192
Amount of CPU Cores: 24
Hard Disk Capacity in GB: 8192
Type of Computing Platform: laptop
VRam Capacity in GB: 24
Total punctuation: 209.184
--------------------------------------------

Output of `allBrandsMostCommonPrintList/0`
--------------------------------------------
Trademark | (CPU manufacturer, GPU manufacturer) | Total amount of counts 
--------------------------------------------
('ASUS',(amd,nvidia),3)
--------------------------------------------
('Dell',(intel,nvidia),6)
--------------------------------------------
('HP',(intel,nvidia),6)
--------------------------------------------
('Lenovo',(intel,nvidia),4)
--------------------------------------------
('MSI',(intel,nvidia),9)
--------------------------------------------
('Razer',(intel,nvidia),3)
--------------------------------------------
('RedMagic',(qualcomm,qualcomm),2)
--------------------------------------------

Output of `ramVRamMostCommonComboPrint/0`
--------------------------------------------
The most common combination of Ram and VRam 
capacity in GB is respectively: 32 and 8 with: 7 apparitions between all the 
computing platforms.
--------------------------------------------

Output of `worstComputingPlatformPrint/0`
--------------------------------------------
The least powerful computing platform before 2023 is the: 
Blade Stealth 13
TradeMark: Razer
Serial number: RZRU1E5P
Ram Capacity in GB: 16
Amount of CPU Cores: 4
Hard Disk Capacity in GB: 512
Type of Computing Platform: laptop
VRam Capacity in GB: 0
Total punctuation: 19.872
--------------------------------------------
```
## Prerequisites to execute this repository
- SWI‑Prolog (recommended >= 8.0). https://www.swi-prolog.org/Download.html
- Visual Studio Code. https://code.visualstudio.com/download
- Have stablished and set up everything for Prolog

## Where to Use
- It can be used in the Prolog own terminal or preferably use the
powershell or VSC code terminal for better watch of code and execution.

## How to Use
- Watch the bash template to see how to execute every query.
   
Made by: Mateo Montoya Ospina and Juan Pablo Lopez Lidueña

```bash
# 1) Clone and enter the repo
git clone The-G-Man-Half-Life/practice2-logical-programming-prolog
cd practice2-logical-programming-prolog

    # 2) enter challenge 1 database queries
    cd challenge1_database
    
    # To use both queries and complex queries
    swipl main.pl
    
    # To execute the queries
    ?- amdAfter2021PrintList.
    ?- amountOfCPASUSPrintList.
    ?- cpHDBetweenPrintList.
    ?- laptopsRamHDPrintList.
    ?- tablet2PlusGBRamPrintList.
    
    # To execute the complex queries
    ?- bestComputingPlatformPrint.
    ?- allBrandsMostCommonPrintList.
    ?- ramVRamMostCommonComboPrint.
    ?- worstComputingPlatformPrint.
    
    # To go back to the root :
    ctrl + c
    press e to exit
    cd ..
'''

