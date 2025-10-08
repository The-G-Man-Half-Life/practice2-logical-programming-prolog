/* 
This is the exportation module which will be used to export all the functions
so they can be used in the main more easily.
*/
:- module(mostCommRamVRamInGBComb, [
    onlyRamVRamStructure/2,
    onlyRamVRamCombosList/1,
    mostCommonCombo/2,
    ramVRamMostCommonComboPrint/0
]).
/* 
importation of facts from the facts archive indicating the type of the facts
and the amount of information coming from them.
*/
:- use_module('../facts', [computingPlatform/11]).

/*
------------------------------------------------------------------------------
onlyRamVRamStructure
What does it do?: It takes care of making a new kind of organization for the
facts now it will only take care of extracting the Ram and VRam capacity
in GB from every computing platform.

Inputs:
It receives all the data of Computing Platforms from facts.pl but only takes
into account these informations of every fact:
- RamCapacityGB as an integer.
- VRamCapacityGB as an integer.
Outputs:
For every computing platform returns:
- RamCapacityGB as an integer.
- VRamCapacityGB as an integer.
------------------------------------------------------------------------------
*/
onlyRamVRamStructure(RamCapacityGB,VRamCapacityGB) :-
    computingPlatform(_, _, _, _, RamCapacityGB, _,_, _,
                      _, _, VRamCapacityGB).
    
/*
------------------------------------------------------------------------------
onlyRamVRamCombosList
What does it do?: It takes care of collecting all the computing platforms 
using the new structure that comes from the query onlyRamVRamStructure, and 
putting all of them in a list where the information is encapsulated in the 
list. Where each Ram and VRam combination will be saved in the list.

Inputs:
A function findall() with the next data:
    - Format in which each computing platform will be encapsulated.
    - New structure in which all computing platforms will be saved now.
    - Way in which the facts will be encapsulated (as a list).

Output:
A list with all of the possible Ram and VRam combinations obtained 
from the facts in the order "(RamCapacityGB,VRamCapacityGB)".
------------------------------------------------------------------------------
*/
onlyRamVRamCombosList(List) :-
    findall(
        (RamCapacityGB,VRamCapacityGB),
        onlyRamVRamStructure(RamCapacityGB,VRamCapacityGB),
        List
    ).
    
/*
------------------------------------------------------------------------------
countOcurrences
What does it do?: It takes care of counting the total amount of apparitions
of a certain Ram/VRam combo for each brand using the list obtained
from onlyRamVRamCombosList all this implementing recursion .

Inputs:
-The list of the Ram/VRam combos in GB obtained from RamVRamCombosList.

Output:
A list with the total count of the total amount of apparitions of every Ram/VRam
combo in the order:
"((RamCapacity/VRamCapacity),count)".
------------------------------------------------------------------------------
*/
countOccurrences([], []).
countOccurrences([H|T], [(H,N)|Rest]) :-
    countOccurrences(T, Rest1),
    ( select((H,N0), Rest1, Rest2) ->
        N is N0 + 1,
        Rest = [(H,N)|Rest2]
    ;   N = 1,
        Rest = [(H,1)|Rest1]
    ).

/*
---------------------------------------------------------------------------------
mostCommonCombo
What does it do?: It takes care of joining onlyRamVRamCombosList, countOccurrences,
and sort. In this way the first function creates the list which then wil be used
for the second function to return all the RamVRam combinations with the total 
amount of counts of each certain combo and finally sort takes care of
sorting all the list elements from countOcurrences list using the second element
,the count, as the sorting factor and finally sorting will only take the head 
element of the sort as it is the Ram/VRam combination with the biggest count so
it can return the combination with the highest count and the count.

Inputs:
Three external functions with the next parameters:
    onlyRamVRamCombosList:
        O: The list with all of the Ram/VRam combos possible from the 
           facts file.
    countOccurences:
        I: The list from onlyRamVRamCombosList.
        O: A new list but only with unique Ram/VRam combos and the total amount 
           of apparitions of that certain combo.
    sort:
        I: The list generated from countOccurrences.
        O: The Ram/VRam combination with the highest count and the count.


Output:
The combo with the highest count and the obtained count.
------------------------------------------------------------------------------
*/
mostCommonCombo(Combo, Count) :-
    onlyRamVRamCombosList(List),
    countOccurrences(List, Counted),
    sort(2, @>=, Counted, [(Combo, Count)|_]).


/*
------------------------------------------------------------------------------
RamVRamMostCommonCombosPrint
What does it do?: It takes care of printing the most common combo of Ram/VRam
found among all the computing platforms using several writelns to display it
properly. 

Inputs:
- The Ram/VRam combination with the highest count.
- The count of that combination. 
------------------------------------------------------------------------------
*/
ramVRamMostCommonComboPrint :-
    mostCommonCombo((Ram,VRam), Count),
    writeln('--------------------------------------------'),
    write('The most common combination of Ram and VRam \ncapacity in GB is respectively: '),
    write(Ram), write(' and '), writeln(VRam), write('with: '), write(Count)
    , writeln( ' apparitions between all the \ncomputing platforms.'),
    writeln('--------------------------------------------').
