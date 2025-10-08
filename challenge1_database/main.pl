/* 
These are the importations modules which will be used to import all the 
functions and queries from the queries to make it easier to use.
*/
:- use_module('./src/queries/amdAfter2021', [
    amdAfter2021/4,
    amdAfter2021List/1,
    amdAfter2021PrintList/0
]).

:- use_module('./src/queries/amountOfCPASUS', [
    amountOfCPASUS/4,
    amountOfCPASUSList/1,
    amountOfCPASUSPrintList/0
]).

:- use_module('./src/queries/cpHDBetween', [
    cpHDBetween/4,
    cpHDBetweenList/1,
    cpHDBetweenPrintList/0
]).

:- use_module('./src/queries/laptopsRamHD', [
    laptopsRamHD/4,
    laptopsRamHDList/1,
    laptopsRamHDPrintList/0
]).

:- use_module('./src/queries/tablet2PlusGBRam', [
    tablet2PlusGBRam/4,
    tablet2PlusGBRamList/1,
    tablet2PlusGBRamPrintList/0
]).

/* 
These are the importations modules which will be used to import all the 
functions and queries from the complex queries to make it easier to use.
*/

:- use_module('./src/complexQueries/bestCPAfter2023', [
    restructuratedCP/2,
    restructuratedCPList/1,
    maximumComputingPlatform/3,
    findBestCPAfter2023/9,
    bestComputingPlatformPrint/0
]).

:- use_module('./src/complexQueries/mostCommCPUGPUManComb', [
    onlyBrandsStructure/3,
    onlyBrandsCombosList/2,
    mostCommonCombo/3,
    allBrandsMostCommon/1,
    allBrandsMostCommonPrintList/0
]).

:- use_module('./src/complexQueries/mostCommRamVRamInGBComb', [
    onlyRamVRamStructure/2,
    onlyRamVRamCombosList/1,
    mostCommonCombo/2,
    ramVRamMostCommonComboPrint/0
]).

:- use_module('./src/complexQueries/worstCPBefore2023', [
    restructuratedCP/2,
    restructuratedCPList/1,
    lowestComputingPlatform/3,
    findWorstCPBefore2023/9,
    worstComputingPlatformPrint/0
]).