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
account the Computing platforms from after 2023.

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
    YearOfAcq > 2023,
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
    - New structure in which all computing platforms will be saved no.
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
maximumComputingPlatform
What does it do?: It takes care of finding the computing platform with the 
highest punctuation so it can determine which device is the best using recursion. 

Inputs:
- The list from restructuratedCPList.

Output:
- A restructuratedCP element that is the Computing platform with the highest
punctuation.
------------------------------------------------------------------------------
*/
maximumComputingPlatform([(Serial, Punctuation)], Serial, Punctuation).

maximumComputingPlatform([(SerialSub1, PunctuationSub1)|Rest], SerialMax, PunctuationMax) :-
    maximumComputingPlatform(Rest, SerialSub2, PunctuationSub2),
    (PunctuationSub1 > PunctuationSub2 ->
        SerialMax = SerialSub1, PunctuationMax = PunctuationSub1
    ;
        SerialMax = SerialSub2, PunctuationMax = PunctuationSub2
    ).

/*
------------------------------------------------------------------------------
findBestCP2023After
What does it do?: It takes care of connecting restructuratedCPList,
maximumComputingPlatform and computing platform so it can obtain obtain all of
the data related to the best computing platform. 

Inputs:
It receives 3 queries and each one takes care of generating inputs and outputs
to cooperate with each other.
restructuratedCPList takes care of generating the list with the new format of
the computing platform.
maximumComputingPlatform takes care of receiving the list with the new format
to return the serial number and the punctuation obtained
and computingPlatform that takes care of returning all the information except
CPUManufacturer, YearOfAcquisition and GPU Manufacturer as these informations
were not taken in count for the punctuation.

Outputs:
For the top computing Platform returns this info:
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
findBestCP2023After(Trademark, Name, Serial, RamCapacityGB, AmountOfCPUCores,
                    HDCapacityGB, TypeOfCP, VRamCapacityGB, Punctuation) :-
    restructuratedCPList(List),
    maximumComputingPlatform(List, Serial, Punctuation),
    computingPlatform(Trademark, Name, Serial, _, RamCapacityGB, _, 
                      AmountOfCPUCores, HDCapacityGB, TypeOfCP, _, 
                      VRamCapacityGB).

/*
------------------------------------------------------------------------------
bestComputingPlatformPrint
What does it do?: It takes care of printing all the information of the compu-
ting platform with the best punctuation.

Inputs:
- The Computing platform with the best punctuation.
------------------------------------------------------------------------------
*/
bestComputingPlatformPrint :-
    findBestCP2023After(TradeMark, Name, Serial, RamCapacityGB,AmountOfCPUCores,HDCapacityGB, TypeOfCP, VramCapacityGB,Punctuation),
    writeln('--------------------------------------------'),
    writeln('The most powerful computing platform is the: '),
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
