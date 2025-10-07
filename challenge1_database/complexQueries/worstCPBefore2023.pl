/* 
importation of facts from the facts archive indicating the type of the facts
and the amount of information coming from them.
*/
:- use_module('../facts', [computingPlatform/11]).

/*
------------------------------------------------------------------------------
restructuratedCP
What does it do?: It takes care of making a restructuring of the computing 
platforms now it has a new sequence for every element of the computing platforms
based on its Serial number and a punctuation obtained from multiplying an impor-
tance rate with every information considered important and then summing the total 
amount of points that were obtained during the procces and then finally dividing
the total with 100 to reduce the size of the value. Also this query only takes into
account the Computing platforms from before 2023.

Inputs:
It receives all the data of Computing Platforms from facts.pl but only takes
into account these informations of every fact:
- Serial as an atom.
- YearOfAcq as an integer.
- RamCapacityGB as an integer.
- Amount of CPU cores as an integer.
- HDCapacityGB as an integer. 
- VRamCapacityGB as an integer.

Outputs:
For every computing platform returns:
- Serial as an atom.
- Punctuation as a double.
------------------------------------------------------------------------------
*/
restructuratedCP(Serial, Punctuation) :-
    computingPlatform(_, _, Serial, YearOfAcq, RamCapacityGB, _,
                      AmountOfCPUCores, HDCapacityGB, _, _, VRamCapacityGB),
    YearOfAcq < 2023,
    Punctuation is (RamCapacityGB * 1.2 + AmountOfCPUCores * 300 + HDCapacityGB * 1.5
                   + VRamCapacityGB * 50)/100.
    
/*
------------------------------------------------------------------------------
restructuratedCPList
What does it do?: It takes care of collecting all the computing platforms using
the new structure that comes from the query restructuratedCP, and putting all of 
them in a list where the information is encapsulated in the format: 
"(Serial,Punctuation)".

Inputs:
A function findall() with the next data:
    - Format in which each computing platform will be encapsulated.
    - New structure in which all computing platforms will be saved now.
    - Way in which the facts will be encapsulated (as a list).

Output:
A list with all the computing platforms according to the new structure where 
each computing platforms is in the format:
"(Serial,Punctuation)".
------------------------------------------------------------------------------
*/
restructuratedCPList(List) :-
    findall(
        (Serial,Punctuation),
        restructuratedCP(Serial, Punctuation),
        List
    ).

/*
------------------------------------------------------------------------------
lowestComputingPlatform
What does it do?: It takes care of finding the computing platform with the 
lowest punctuation so it can determine which device is the worst using recursion. 

Inputs:
- The list from restructuratedCPList.

Output:
- A restructuratedCP element that is the Computing platform with the lowest
punctuation.
------------------------------------------------------------------------------
*/
lowestComputingPlatform([(Serial, Punctuation)], Serial, Punctuation).

lowestComputingPlatform([(SerialSub1, PunctuationSub1)|Rest], SerialLow, PunctuationLow) :-
    lowestComputingPlatform(Rest, SerialSub2, PunctuationSub2),
    (PunctuationSub1 < PunctuationSub2 ->
        SerialLow = SerialSub1, PunctuationLow = PunctuationSub1
    ;
        SerialLow = SerialSub2, PunctuationLow = PunctuationSub2
    ).

/*
------------------------------------------------------------------------------
findworstCP2023
What does it do?: It takes care of connecting restructuratedCPList,
lowestComputingPlatform and computingPlatform so it can obtain obtain all of
the data related to the worst computing platform. 

Inputs:
It receives 3 queries and each one takes care of generating inputs and outputs
to cooperate with each other.
restructuratedCPList takes care of generating the list with the new format of
the computing platform.
lowestComputingPlatform takes care of receiving the list with the new format
to return the serial number and the punctuation obtained
and computingPlatform that takes care of returning all the information except
CPUManufacturer, YearOfAcquisition and GPU Manufacturer as these informations
were not taken in count for the punctuation.

Outputs:
For the lowest computing Platform returns this info:
- Trademark as an atom.
- Name as an atom.
- Serial as an atom.
- RamCapacityGB as an integer.
- AmountOfCPUCores as an integer.
- HDCapacityGB as an integer. 
- TypeOfCP as an atom
- VRamCapacityGB as an integer.
- Punctuation as a double.
------------------------------------------------------------------------------
*/
findWorstCPBefore2023(Trademark, Name, Serial, RamCapacityGB, AmountOfCPUCores,
                    HDCapacityGB, TypeOfCP, VRamCapacityGB, Punctuation) :-
    restructuratedCPList(List),
    lowestComputingPlatform(List, Serial, Punctuation),
    computingPlatform(Trademark, Name, Serial, _, RamCapacityGB, _, 
                      AmountOfCPUCores, HDCapacityGB, TypeOfCP, _, 
                      VRamCapacityGB).

/*
-----------------------------------------------------------------------------
worstComputingPlatformPrint
What does it do?: It takes care of printing all the information of the compu-
ting platform with the worst punctuation.

Inputs:
- The Computing platform with the worst punctuation.
------------------------------------------------------------------------------
*/
worstComputingPlatformPrint :-
    findWorstCPBefore2023(TradeMark, Name, Serial, RamCapacityGB,AmountOfCPUCores,HDCapacityGB, TypeOfCP, VramCapacityGB,Punctuation),
    writeln('--------------------------------------------'),
    writeln('The least powerful computing platform before 2023 is the: '),
    writeln(Name),
    write('TradeMark: '),
    writeln(TradeMark),
    write('Serial number: '),
    writeln(Serial),
    write('Ram Capacity in GB: '),
    writeln(RamCapacityGB),
    write('Amount of CPU Cores: '),
    writeln(AmountOfCPUCores),
    write('Hard Disk Capacity in GB: '),
    writeln(HDCapacityGB),
    write('Type of Computing Platform: '),
    writeln(TypeOfCP),
    write('VRam Capacity in GB: '),
    writeln(VramCapacityGB),
    write('Total punctuation: '),
    writeln(Punctuation),
    writeln('--------------------------------------------').
