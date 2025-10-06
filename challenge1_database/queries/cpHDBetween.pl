/* 
importation of facts from the facts archive indicating the type of the facts
and the amount of information coming from them.
*/
:- use_module('../facts', [computingPlatform/11]).

/*
------------------------------------------------------------------------------
cpHDBetween
What does it do?: It takes care of finding all the computing platforms where
the hard disk capacity is between 32 and 512 GB.

Inputs:
It receives all the data of Computing Platforms from facts.pl but only takes
into account these informations of every fact:
- Trademark as an atom.
- Name as an atom.
- Serial as an atom.
- HDCapacityGB as an integer.

Outputs:
For every computing platform returns:
- Trademark as an atom.
- Name as an atom.
- Serial as an atom.
- HDCapacityGB as an integer.
------------------------------------------------------------------------------
*/
cpHDBetween(Trademark, Name, Serial, HDCapacityGB) :-
    computingPlatform(Trademark, Name, Serial, _, _, _, _, HDCapacityGB, _, _, _),
    HDCapacityGB > 32,
    HDCapacityGB < 512.
/*
------------------------------------------------------------------------------
cpHDBetweenList
What does it do?: It takes care of finding all the computing platforms using
the conditions that comes from the query cpHDBetween, and putting all of 
them in a list where the information is encapsulated in the format: 
"(Trademark|Name|Serial|HDCapacityGB)".

Inputs:
A function findall() with the next data:
    - Format in which each computing platform will be encapsulated.
    - Condition or goal which will be used to find the computing platforms.
    - Way in which the facts will be saved (as a list).

Output:
A list with all the computing platforms according to the conditions where 
each computing platforms is in the format:
"(Trademark|Name|Serial|HDCapacityGB)".
------------------------------------------------------------------------------
*/
cpHDBetweenList(List) :-
    findall(
        (Trademark|Name|Serial|HDCapacityGB),
        cpHDBetween(Trademark, Name, Serial,HDCapacityGB),
        List
    ).

/*
------------------------------------------------------------------------------
cpHDBetweenPrintList
What does it do?: It takes care of printing all the computing platforms with 
a header indicating the order in which the information is displayed and a se-
paration line to distinguish better each computing platform.

Inputs:
- The list from cpHDBetweenList
- A subfunction that takes care of printing each computing platform from the 
cpHDBetweenList.
------------------------------------------------------------------------------
*/
cpHDBetweenPrintList :-
    cpHDBetweenList(List),
    writeln('--------------------------------------------'),
    writeln('Trademark | Name | Serial number | Hard disk capacity in GB'),
    printList(List),
    writeln('--------------------------------------------').


/*
------------------------------------------------------------------------------
printList
What does it do?: It takes care of printing all the computing platforms passed
in the list using recursion.

Inputs:
- The list from cpHDBetweenList.
------------------------------------------------------------------------------
*/
printList([]).
printList([X|Xs]) :-
    writeln('--------------------------------------------'),
    writeln(X),
    printList(Xs).