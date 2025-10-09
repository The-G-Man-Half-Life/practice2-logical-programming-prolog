# Practice Number 2 - Logical Programming in ProLog

This project contains 2 distinct modules both having distinct knowledge databases and different queries that try to answer some common petitions that an user could have related to the information contained inside the knowledge databases being the first focused on computing platforms and the second one focused on trips and travels.

## Contents

- `challenge1_database`
  - `src`
    - `complexQueries`
      - 
    - `queries` 
  - `P3_2_NumericalMethods/`
    - `cosineFunction.hs` 
    - `exponentialFunction.hs` 
    - `naturalLogarithm.hs`
  - `P3_3_DSPT/`
    - `discreteCosine.hs` 
- `documents/`
    - `cosineTest.hs` 
    - `expTest.hs` 
    - `naturalLogarithmTest.hs`
    - `DCTTest.hs`
- `Video/` 
    - `VideoLink.txt`
- `README.md` 

## Program Functionalities

### 1) challenge1_database

  #### complexQueries
- **bestCPAfter2023.pl**
-  takes care of finding the best computing platform after 2023 from facts.pl.
- **mostCommCPUGPUManComb.pl**
-  takes care of finding the most common CPU/GPU manufacturer combinations
   for each trademark using the data from facts.pl.
- **mostCommRamVRamInGBComb.pl**
-  takes care of finding the most common Ram/VRam in GB combinatio using the
   data from facts.pl.
- **worstCPBefore2023.pl**
-  takes care of finding the worst computing platform before 2023 from facts.pl. 

  #### queries
- **amdAfter2021.pl**
-  takes care of finding the best computing platform after 2023 from facts.pl.
- **amountOfCPASUS.pl**
-  takes care of finding the best computing platform after 2023 from facts.pl.
- **amdAfter2021.pl**
-  takes care of finding the best computing platform after 2023 from facts.pl.
- **amdAfter2021.pl**
-  takes care of finding the best computing platform after 2023 from facts.pl.
- **amdAfter2021.pl**
-  takes care of finding the best computing platform after 2023 from facts.pl.

### 2) P3_2_NumericalMethods
- **cosineFunction.hs**  
  Calculates `cos(x)` by summing a finite number of terms of its series.  

- **exponentialFunction.hs**  
  Calculates `e^x` with a multiple summatory.  

- **naturalLogarithm.hs**  
  Approximates `ln(1 + x)` (valid for `-1 < x ≤ 1`).  

## Problems and errors during the development

### 1) P3_1_UsefulFunctions
- **removeData.hs**  
  - Problems related to what could happen if
    the minimal value was bigger than the max
    one.
  - Deciding wether or not should be used a
    comprehensive list over a basic function
    to make the program.
  
- **sortArray.hs**  
   - Finding a proper way to make the cycle
     as this was the first program to be made.
   - Understanding how to organize the flow
     of the multiple functions as this program
     required multiple verifications throught
     the execution of the program.
   - Recognizing how to swap the multiple
     elements in a proper way.

### 2) P3_2_NumericalMethods
- **cosineFunction.hs**  
   - Handling how fast were the numbers increasing
     due to the cosine inside the program because
     these values could return infinite or crash
     the device.
   - Organizing the functions to make the main
     function execute properly.
   - Making that the tests run properly.
     
- **exponentialFunction.hs**  
   - Problems related to how to divide properly
     the multiple functions involucrated in the
     main function.
   - Problems related to what could happen with
     negative exponents.
   - Problems related to how to include constant
     values without requesting them from users.
   - Problem related about deciding the stopping
     functions for the auxiliar functions.
   - Understanding how to reduce pi properly.
     
- **naturalLogarithm.hs**  
   - Organizing properly the functions.

### 3) P3_3_DSPT
- **discreteCosine.hs**  
    - Problems related on understanding the needed
      functions for the program as it was very
      complex.
    - Problems related to understanding how to use
      properly the lists for this program.
    - Problem related to deciding how to obtain the
      square root with the basic elements of haskell
      and minimizing the error rate we could obtain.
    - Wether or not was worth using double over float
      to reduce the marginal error we could obtain.
    - How to organize properly the flow of the functions
      to make everything work properly having in mind the
      many functionalities present in the program.
      
### General problems throught the development

   - Installing all the required components to
     make Haskell compile properly since we
     were using VSC to write and test everything.
   - The high difficulty involved in understanding
     Haskell to develop all the programs since our
     team has not had to deal with functional
     programming ever.
   - Coordinating all the proccess through Github.
     Since we divided the work between all members
     coordinating how to use the tool appropiately
     to evite fatal mistakes was really important.
   - Time organization and video preparation.
   - Maintaining a clean code throught the entire
     development proccess
     
## Conclusions

  - The development of this project taught us the im-
    portance of learning and adapting to new languages
    to sort all kind of problems and difficulties.
  - It is really important to understand how to sepa
    rate properly all the functionalities in your pro-
    gram to maintain the order and cohesion and evite
    problems.
  - It is necessary to understand the use of tools
    such as Github and VSC to develop and test your 
    code properly.
  - The teamwork was really important in this project
    as We helped each other and learned tons of things
    of each other.

## Notes
  - To make more emphasis on the development and expla-
    nation of the code we decided to focus the video
    only on that and move the tests to the document
    section in the repository and the mistakes and
    problems appear in this read me.

## Prerequisites to execute this repository

- GHC (Haskell Compiler)
- Optional "Cabal" for project management
- Optional Code Editor (VS Code)

## Where to Use

In PowerShell

## How to Use

In PowerShell:

Hecho por: Mateo Montoya Ospina y Juan Pablo Lopez Lidueña

```bash
# 1) Clone and enter the repo
gh repo clone The-G-Man-Half-Life/PracticaNumero1ProgramacionFuncionalEnHaskell
cd PracticaNumero1ProgramacionFuncionalEnHaskell

# 2) P3_1_UsefulFunctions
cd Src\P3_1_UsefulFunctions

# To run the tests of removeData.hs, type in console:
runhaskell removeData.hs

# To run the tests of sortArray.hs, type in console:
runhaskell sortArray.hs

# To go back to the Src directory:
cd ..

# 3) P3_2_NumericalMethods
cd Src\P3_2_NumericalMethods

# To run the tests of cosineFunction.hs, type in console:
runhaskell cosineFunction.hs

# To run the tests of exponentialFunction.hs, type in console:
runhaskell exponentialFunction.hs

# To run the tests of naturalLogarithm.hs, type in console:
runhaskell naturalLogarithm.hs

# To go back to the Src directory:
cd ..

# 4) P3_3_DSPT
cd Src\P3_3_DSPT

# To run the tests of discreteCosine.hs, type in console:
runhaskell discreteCosine.hs

# To go back to the Src directory:
cd ..
