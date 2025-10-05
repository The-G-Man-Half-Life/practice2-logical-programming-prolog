/* 
importation of facts from the facts archive indicating the type of the facts
and the amount of information coming from them.
*/
:- use_module(facts, [computingPlatform/11]).

/*
------------------------------------------------------------------------------
amd_after_2021
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
amd_after_2021(Trademark, Name, Serial, TypeOfCP) :-
    computingPlatform(Trademark, Name, Serial, YearOfAcq, _, CPUManufacturer, _, _, TypeOfCP, _, _),
    YearOfAcq > 2021,
    CPUManufacturer == amd.

/*
------------------------------------------------------------------------------
amd_after_2021_list
What does it do?: It takes care of finding all the computing platforms using
the conditions that come from the query amd_after_2021, and putting all of 
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
amd_after_2021_list(List) :-
    findall(
        (Trademark|Name|Serial|TypeOfCP),
        amd_after_2021(Trademark, Name, Serial, TypeOfCP),
        List
    ).

/*
------------------------------------------------------------------------------
amd_after_2021_print_list
What does it do?: It takes care of printing all the computing platforms with 
a header indicating the order in which the information is displayed and a se-
paration line to distinguish better each computing platform.

Inputs:
- The list from amd_after_2021_list
- A subfunction that takes care of printing each computing platform from the 
amd_after_2021_list.
------------------------------------------------------------------------------
*/
amd_after_2021_print_list :-
    amd_after_2021_list(List),
    writeln('--------------------------------------------'),
    writeln('Trademark | Name | Serial number | Type of CP'),
    print_list(List),
    writeln('--------------------------------------------').


/*
------------------------------------------------------------------------------
print_list
What does it do?: It takes care of printing all the computing platforms passed
in the list using recursion

Inputs:
- The list from amd_after_2021_list
------------------------------------------------------------------------------
*/
print_list([]).
print_list([X|Xs]) :-
    writeln('--------------------------------------------'),
    writeln(X),
    print_list(Xs).