/* 
importation of facts from the facts archive indicating the type of the facts
and the amount of information coming from them.
*/
:- use_module('../facts', [computingPlatform/11]).

/*
------------------------------------------------------------------------------
onlyBrandsStructure
What does it do?: It takes care of making a new kind of organization for the
facts now it will only take care of extracting the Trademark, CPU manufacturer
and GPU manufacturer from every computing platform.

Inputs:
It receives all the data of Computing Platforms from facts.pl but only takes
into account these informations of every fact:
- Trademark as an atom.
- CPUManu as an atom.
- GPUManu as an atom.
Outputs:
For every computing platform returns:
- Trademark as an atom.
- CPUManu as an atom.
- GPUManu as an atom.
------------------------------------------------------------------------------
*/
onlyBrandsStructure(Trademark,CPUManu,GPUManu) :-
    computingPlatform(Trademark, _, _, _, _, CPUManu,_, _,
                      _, GPUManu, _).
    
/*
------------------------------------------------------------------------------
onlyBrandsCombosList
What does it do?: It takes care of collecting all the computing platforms 
using the new structure that comes from the query onlyBrandsStructure, and 
putting all of them in a list where the information is encapsulated in the 
format: "(Trademark,List)". Where each CPUMan and GPUMan combination will be
saved being this list related to the trademark where it comes from.

Inputs:
A function findall() with the next data:
    - Format in which each computing platform will be encapsulated.
    - New structure in which all computing platforms will be saved now.
    - Way in which the facts will be encapsulated (as a list).

Output:
A list with all of the of the trademarks each one being related to a list
wil all the CPU/GPU Manufacturers combination found in the facts in the
structure: "(Trademark,List)".
------------------------------------------------------------------------------
*/
onlyBrandsCombosList(Trademark,List) :-
    findall(
        (CPUManu,GPUManu),
        onlyBrandsStructure(Trademark,CPUManu,GPUManu),
        List
    ).
    
/*
------------------------------------------------------------------------------
countOcurrences
What does it do?: It takes care of counting the total amount of apparitions
of a certain CPU/GPU manufacturer combo for each brand using the list obtained
from onlyBrandsCombosList all this implementing recursion .

Inputs:
-The list of the CPUManufacturer and GPUManufacturer combos of the brand.

Output:
A list with the total count of the total amount of apparitions of every CPU/
GPU Manufacturer combo in the order:
"((CPUManufacturer/GPUManufacturer),count)".
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
What does it do?: It takes care of joining onlyBrandsComboList, countOccurrences,
and sort. In this way the first function creates the list which then wil be used
for the second function to return all the CPU/GPU manufacturers combinations with
the total amount of counts of that certain combo and finally sort takes care of
sorting all the list elements from countOcurrences list using the second element
,the count, as the sorting factor and finally sorting will only take the head 
element of the sort as it is the CPU/ GPU Manufacturer combination with the 
biggest count so it can return the trademark with the combo with the highest 
count and the count. 

Inputs:
Three external functions with the next parameters:
    onlyBrandsCombosList:
        O: The trademark which is wished to find the most common combo,
           The list with all of the CPU/GPU Combination.
    countOccurences:
        I: The list from onlyBrandsCombosList.
        O: A new list but only with unique CPU/GPU combos and the total amount 
           of apparitions.
    sort:
        I:The list generated from countOccurrences.
        O: The CPU/GPU Manufacturer combination with the highest count.


Output:
The Trademark with the combo with the highest count and the obtained count.
------------------------------------------------------------------------------
*/
mostCommonCombo(Trademark, Combo, Count) :-
    onlyBrandsCombosList(Trademark, List),
    countOccurrences(List, Counted),
    sort(2, @>=, Counted, [(Combo, Count)|_]).

/*
------------------------------------------------------------------------------
allBrandsMostCommon
What does it do?: It takes care of returning a list of lists where each mini-
list contains the trademark the CPU/GPU Manufacturer with the biggest count
of that trademark and the total count of the apparitions of that brand.
and for obtaining this list of mini-lists is implemented seof() to obtain
all the Trademarks in a list and findall() to make the list of mini-lists
using member() as the format relating each brand of the list from setof
and its respective trademark with the combo and the count all this to finally
return the list of mini-lists.

Inputs:
Three functions that cooperate with each other:
    setof():
            I:an indication of only receiving the Trademarks from all the
              facts using the first part as indication to ignore everything
              except Trademark.
            O: A list of all the trademarks renamed as brands.
    findall():
            I: The Brands list from setof(),a member() function to take
               care of relationating trademark with the most common combo
               and count, the mostCommonCombo function to obtain the most-
               CommonCombo and the total amount of apparitions
            O:The FinalList with each Trademark and the most common CPU/GPU
              Manufacturer Combo and the amount of apparitions

Output:
A list with all the computing platforms according to the new structure where 
each computing platforms is in the format:
"(Serial,Punctuation)".
------------------------------------------------------------------------------
*/
allBrandsMostCommon(FinalList) :-
    setof(Trademark, Name^Serial^YearOfAcq^Ram^CPU^Cores^HD^TypeCP^GPU^VRAM^
          computingPlatform(Trademark, Name, Serial, YearOfAcq, Ram, CPU,
                            Cores, HD, TypeCP, GPU, VRAM),
          Brands),
    findall((Trademark, Combo, Count),
            (member(Trademark, Brands), mostCommonCombo(Trademark, Combo, Count)),
            FinalList).

/*
------------------------------------------------------------------------------
allBrandsMostCommonPrintList 
What does it do?: It takes care of printing a list with all the Trademarks,
best CPU/GPU manufacturers combo for each brand and the total amount of 
apparitions of that certain combo, with a header indicating the order in which 
the information is displayed and a separation line to distinguish better each
information. 

Inputs:
- The list from allbrandsMostCommon.
- A subfunction that takes care of printing the information. 
------------------------------------------------------------------------------
*/
allBrandsMostCommonPrintList :-
    allBrandsMostCommon(FinalList),
    writeln('--------------------------------------------'),
    writeln('Trademark | (CPU manufacturer, GPU manufacturer) | Total amount of counts '),
    printList(FinalList),
    writeln('--------------------------------------------').


/*
------------------------------------------------------------------------------
printList
What does it do?: It takes care of printing all the minilist elements passed
in the list using recursion.

Inputs:
- The list from allBrandsMostCommon.
------------------------------------------------------------------------------
*/
printList([]).
printList([X|Xs]) :-
    writeln('--------------------------------------------'),
    writeln(X),
    printList(Xs).
