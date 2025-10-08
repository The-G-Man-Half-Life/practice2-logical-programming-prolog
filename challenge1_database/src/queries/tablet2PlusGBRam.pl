/* 
This is the exportation module which will be used to export all the functions
so they can be used in the main more easily
*/
:- module(tablet2PlusGBRam, [
    tablet2PlusGBRam/4,
    tablet2PlusGBRamList/1,
    tablet2PlusGBRamPrintList/0
]).
/* 
importation of facts from the facts archive indicating the type of the facts
and the amount of information coming from them.
*/
:- use_module('../facts', [computingPlatform/11]).
/*
------------------------------------------------------------------------------
tablet2PlusGBRam
What does it do?: It takes care of finding all the computing platforms where
the Type of computing platform is a tablet and the Ram capcity installed is 
over 2 Gyga Bytes.

Inputs:
It receives all the data of Computing Platforms from facts.pl but only takes
into account these informations of every fact:
- Trademark as an atom.
- Name as an atom.
- Serial as an atom.
- RamCapacityGB as an integer.
- TypeOfCP as an atom.

Outputs:
For every computing platform returns:
- Trademark as an atom.
- Name as an atom.
- Serial as an atom.
- RamCapacityGB as an integer.
------------------------------------------------------------------------------
*/
tablet2PlusGBRam(Trademark, Name, Serial, RamCapacityGB) :-
    computingPlatform(Trademark, Name, Serial, _, RamCapacityGB, _, _, _, TypeOfCP, _, _),
    RamCapacityGB > 2,
    TypeOfCP == tablet.
/*
------------------------------------------------------------------------------
tablet2PlusGBRamList
What does it do?: It takes care of finding all the computing platforms using
the conditions that comes from the query tablet2PlusGBRam, and putting all of 
them in a list where the information is encapsulated in the format: 
"(Trademark|Name|Serial|RamCapacityGB)".

Inputs:
A function findall() with the next data:
    - Format in which each computing platform will be encapsulated.
    - Condition or goal which will be used to find the computing platforms.
    - Way in which the facts will be saved (as a list).

Output:
A list with all the computing platforms according to the conditions where 
each computing platforms is in the format:
"(Trademark|Name|Serial|RamCapacityGB)".
------------------------------------------------------------------------------
*/
tablet2PlusGBRamList(List) :-
    findall(
        (Trademark|Name|Serial|RamCapacityGB),
        tablet2PlusGBRam(Trademark, Name, Serial,RamCapacityGB),
        List
    ).

/*
------------------------------------------------------------------------------
tablet2PlusGBRamPrintList
What does it do?: It takes care of printing all the computing platforms with 
a header indicating the order in which the information is displayed and a se-
paration line to distinguish better each computing platform.

Inputs:
- The list from tablet2PlusGBRamList
- A subfunction that takes care of printing each computing platform from the 
tablet2PlusGBRamList.
------------------------------------------------------------------------------
*/
tablet2PlusGBRamPrintList :-
    tablet2PlusGBRamList(List),
    writeln('--------------------------------------------'),
    writeln('Trademark | Name | Serial number | Ram capacity in GB'),
    printList(List),
    writeln('--------------------------------------------').


/*
------------------------------------------------------------------------------
printList
What does it do?: It takes care of printing all the computing platforms passed
in the list using recursion.

Inputs:
- The list from tablet2PlusGBRamList.
------------------------------------------------------------------------------
*/
printList([]).
printList([X|Xs]) :-
    writeln('--------------------------------------------'),
    writeln(X),
    printList(Xs).