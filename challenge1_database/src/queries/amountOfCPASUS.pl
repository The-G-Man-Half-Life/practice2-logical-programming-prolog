/* 
This is the exportation module which will be used to export all the functions
so they can be used in the main more easily.
*/
:- module(amountOfCPASUS, [
    amountOfCPASUS/4,
    amountOfCPASUSList/1,
    amountOfCPASUSPrintList/0
]).
/* 
importation of facts from the facts archive indicating the type of the facts
and the amount of information coming from them.
*/
:- use_module('../facts', [computingPlatform/11]).

/*
------------------------------------------------------------------------------
amountOfCPASUS
What does it do?: It takes care of finding all the computing platforms where
the Trademark is ASUS.

Inputs:
It receives all the data of Computing Platforms from facts.pl but only takes
into account these informations of every fact:
- Trademark as an atom.
- Name as an atom.
- Serial as an atom.
- YearOfAcq as an integer.
- TypeOfCP as an atom. 

Outputs:
For every computing platform returns:
- Name as an atom.
- Serial as an atom.
- YearOfAcq as an integer.
- TypeOfCP as an atom. 
------------------------------------------------------------------------------
*/
amountOfCPASUS(Name, Serial, YearOfAcq, TypeOfCP) :-
    computingPlatform(Trademark, Name, Serial, YearOfAcq, _, _, _, _, TypeOfCP, _, _),
    Trademark == 'ASUS'.
/*
------------------------------------------------------------------------------
amountOfCPASUSList
What does it do?: It takes care of finding all the computing platforms using
the conditions that comes from the query amountOfCPASUS, and putting all of 
them in a list where the information is encapsulated in the format: 
"(Name|Serial|YearOfAcq|TypeOfCP)".

Inputs:
A function findall() with the next data:
    - Format in which each computing platform will be encapsulated.
    - Condition or goal which will be used to find the computing platforms.
    - Way in which the facts will be saved (as a list).

Output:
A list with all the computing platforms according to the conditions where 
each computing platforms is in the format:
"(Name|Serial|YearOfAcq|TypeOfCP)".
------------------------------------------------------------------------------
*/

amountOfCPASUSList(List) :-
    findall(
        (Name|Serial|YearOfAcq|TypeOfCP),
        amountOfCPASUS(Name, Serial, YearOfAcq, TypeOfCP),
        List
    ).

/*
------------------------------------------------------------------------------
amountOfCPASUSPrintList
What does it do?: It takes care of printing the total amount of computing 
platforms from ASUS and a list with all the computing platforms with a header 
indicating the order in which the information is displayed and a se-
paration line to distinguish better each computing platform.

Inputs:
- The list from amountOfCPASUSList.
- The total amount of ASUS Computing platforms from countList.
- A subfunction that takes care of printing each computing platform from the 
amountOfCPASUSList.
------------------------------------------------------------------------------
*/
amountOfCPASUSPrintList :-
    amountOfCPASUSList(List),
    countList(List, N),
    writeln('--------------------------------------------'),
    format('This is the amount of ASUS computing platforms: ~w~n', [N] ),
    writeln('--------------------------------------------'),
    writeln('These are all of the  ASUS computing platforms: '),
    writeln('--------------------------------------------'),
    writeln('Name | Serial number | Year of acquisition | Type of Computing platform'),
    printList(List),
    writeln('--------------------------------------------').


/*
------------------------------------------------------------------------------
printList
What does it do?: It takes care of printing all the computing platforms passed
in the list using recursion.

Inputs:
- The list from amountOfCPASUSList.
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
- The list from amountOfCPASUSList
------------------------------------------------------------------------------
*/
countList([], 0).
countList([_|T], N) :-
    countList(T, N1),
    N is N1 + 1.