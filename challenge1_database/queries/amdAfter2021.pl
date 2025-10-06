/* 
importation of facts from the facts archive indicating the type of the facts
and the amount of information coming from them.
*/
:- use_module('../facts', [computingPlatform/11]).

/*
------------------------------------------------------------------------------
amdAfter2021
What does it do?: It takes care of finding all the computing platforms where
the CPU manufacturer is AMD and the year of acquisition is after 2021.

Inputs:
It receives all the data of Computing Platforms from facts.pl but only takes
into account these informations of every fact:
- Trademark as an atom.
- Name as an atom.
- Serial as an atom.
- Year of acquisition as an integer.
- CPU manufacturer as an atom.
- Type of Computing Platform as an atom.

Outputs:
For every computing platform returns:
- Trademark as an atom.
- Name as an atom.
- Serial as an atom.
- Type of Computing Platform as an atom.
------------------------------------------------------------------------------
*/
amdAfter2021(Trademark, Name, Serial, TypeOfCP) :-
    computingPlatform(Trademark, Name, Serial, YearOfAcq, _, CPUManufacturer, _, _, TypeOfCP, _, _),
    YearOfAcq > 2021,
    CPUManufacturer == amd.

/*
------------------------------------------------------------------------------
amdAfter2021List
What does it do?: It takes care of finding all the computing platforms using
the conditions that comes from the query amdAfter2021, and putting all of 
them in a list where the information is encapsulated in the format: 
"(Trademark|Name|Serial|TypeOfCP)".

Inputs:
A function findall() with the next data:
    - Format in which each computing platform will be encapsulated.
    - Condition or goal which will be used to find the computing platforms.
    - Way in which the facts will be saved (as a list).

Output:
A list with all the computing platforms according to the conditions where 
each computing platforms is in the format:"(Trademark|Name|Serial|TypeOfCP)".
------------------------------------------------------------------------------
*/
amdAfter2021List(List) :-
    findall(
        (Trademark|Name|Serial|TypeOfCP),
        amdAfter2021(Trademark, Name, Serial, TypeOfCP),
        List
    ).

/*
------------------------------------------------------------------------------
amdAfter2021PrintList
What does it do?: It takes care of printing all the computing platforms with 
a header indicating the order in which the information is displayed and a se-
paration line to distinguish better each computing platform.

Inputs:
- The list from amdAfter2021List
- A subfunction that takes care of printing each computing platform from the 
amdAfter2021List.
------------------------------------------------------------------------------
*/
amdAfter2021PrintList :-
    amdAfter2021List(List),
    writeln('--------------------------------------------'),
    writeln('Trademark | Name | Serial number | Type of CP'),
    printList(List),
    writeln('--------------------------------------------').


/*
------------------------------------------------------------------------------
printList
What does it do?: It takes care of printing all the computing platforms passed
in the list using recursion

Inputs:
- The list from amdAfter2021List
------------------------------------------------------------------------------
*/
printList([]).
printList([X|Xs]) :-
    writeln('--------------------------------------------'),
    writeln(X),
    printList(Xs).