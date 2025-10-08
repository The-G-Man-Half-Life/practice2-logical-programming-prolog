/* 
This is the exportation module which will be used to export all the functions
so they can be used in the main more easily
*/
:- module(laptopsRamHD, [
    laptopsRamHD/4,
    laptopsRamHDList/1,
    laptopsRamHDPrintList/0
]).
/* 
importation of facts from the facts archive indicating the type of the facts
and the amount of information coming from them.
*/
:- use_module('../facts', [computingPlatform/11]).
/*
------------------------------------------------------------------------------
laptopsRamHD
What does it do?: It takes care of finding all the computing platforms where
the Ram Capacity is above 4GB of Ram and the Hard disk capacity is under 512
GB.

Inputs:
It receives all the data of Computing Platforms from facts.pl but only takes
into account these informations of every fact:
- Name as an atom.
- Serial as an atom.
- RamCapacityGB as an integer.
- HDCapacityGB as an integer. 

Outputs:
For every computing platform returns:
- Name as an atom.
- Serial as an atom.
- RamCapacityGB as an integer.
- HDCapacityGB as an integer. 
------------------------------------------------------------------------------
*/
laptopsRamHD(Name, Serial, RamCapacityGB, HDCapacityGB) :-
    computingPlatform(_, Name, Serial, _, RamCapacityGB, _, _, HDCapacityGB, _, _, _),
    RamCapacityGB > 4,
    HDCapacityGB < 512.
    
/*
------------------------------------------------------------------------------
laptopsRamHDList
What does it do?: It takes care of finding all the computing platforms using
the conditions that comes from the query laptopsRamHD, and putting all of 
them in a list where the information is encapsulated in the format: 
"(Name|Serial|RamCapacityGB|HDCapacityGB)".

Inputs:
A function findall() with the next data:
    - Format in which each computing platform will be encapsulated.
    - Condition or goal which will be used to find the computing platforms.
    - Way in which the facts will be saved (as a list).

Output:
A list with all the computing platforms according to the conditions where 
each computing platforms is in the format:
"(Name|Serial|RamCapacityGB|HDCapacityGB)".
------------------------------------------------------------------------------
*/

laptopsRamHDList(List) :-
    findall(
        (Name|Serial|RamCapacityGB|HDCapacityGB),
        laptopsRamHD(Name, Serial, RamCapacityGB, HDCapacityGB),
        List
    ).

/*
------------------------------------------------------------------------------
laptopsRamHDPrintList
What does it do?: It takes care of printing the total amount of computing 
platforms with the characteristics and a list with all the computing 
platforms with a header indicating the order in which the information is 
displayed and a separation line to distinguish better each computing platform.

Inputs:
- The list from laptopsRamHDList.
- The total amount of Computing platforms from countList.
- A subfunction that takes care of printing each computing platform from the 
laptopsRamHDList.
------------------------------------------------------------------------------
*/
laptopsRamHDPrintList :-
    laptopsRamHDList(List),
    countList(List, N),
    writeln('--------------------------------------------'),
    format('This is the amount of Computing Platforms with\nmore than 4 GB of Ram and less than 512 GB of \nHard Disk: ~w~n', [N] ),
    writeln('--------------------------------------------'),
    writeln('These are all of the found computing platforms: '),
    writeln('--------------------------------------------'),
    writeln('Name | Serial number | Ram capacity in GB | Hard disk capacity in GB'),
    printList(List),
    writeln('--------------------------------------------').


/*
------------------------------------------------------------------------------
printList
What does it do?: It takes care of printing all the computing platforms passed
in the list using recursion.

Inputs:
- The list from laptopsRamHDList.
------------------------------------------------------------------------------
*/
printList([]).
printList([X|Xs]) :-
    writeln('--------------------------------------------'),
    writeln(X),
    printList(Xs).

/*
------------------------------------------------------------------------------
countList
What does it do?: It takes care of counting the total of computing platforms
passed in the list using recursion.

Inputs:
- The list from laptopsRamHDList
------------------------------------------------------------------------------
*/
countList([], 0).
countList([_|T], N) :-
    countList(T, N1),
    N is N1 + 1.