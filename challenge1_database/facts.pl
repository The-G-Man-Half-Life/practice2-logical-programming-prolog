%importation module
:- module(facts, [computingPlatform/11]).

/*
facts structure:
computingPlatform(Trademark, Name, SerialNumber, YearOfAcquisition, RAMCapacity_GB,
         CPUManufacturer, AmountOfCPUCores, HardDiskCapacity_GB,
         TypeOfComputingPlatform, GPUManufacturer, VRamCapacity_GB).
*/

% Available HP computing platforms
computingPlatform('HP', 'Omen Max 16', 'AH0007NS', 2025, 64, intel, 24, 4096, laptop, nvidia, 24).
computingPlatform('HP', 'Omen 17', '5CDY8B2L', 2024, 64, intel, 24, 2048, laptop, nvidia, 16).
computingPlatform('HP', 'Omen Transcend 16', '5CDW6C3M', 2024, 32, intel, 16, 2048, laptop, nvidia, 12).
computingPlatform('HP', 'Omen 16',     'Wf0083DX', 2024, 16, intel, 24, 1024, laptop, nvidia, 8).
computingPlatform('HP', 'Omen 16', '5CDV9D4N', 2023, 32, amd, 16, 1024, laptop, nvidia, 12).
computingPlatform('HP', 'Omen 15', '5CDU1E5P', 2022, 32, amd, 8, 1024, laptop, nvidia, 8).
computingPlatform('HP', 'Victus 16', '5CDT3F6Q', 2022, 16, intel, 8, 1024, laptop, nvidia, 6).
computingPlatform('HP', 'Pavilion Gaming 15', '5CDS4G7R', 2021, 16, amd, 6, 512, laptop, nvidia, 4).
computingPlatform('HP', 'Pavilion Gaming 15', '5CDR5H8S', 2020, 8, intel, 4, 512, laptop, nvidia, 4).

% Available Lenovo computing platforms
computingPlatform('Lenovo', 'Legion Pro 7i Gen 9', 'LNVX9A1Q', 2025, 64, intel, 24, 2048, laptop, nvidia, 24).
computingPlatform('Lenovo', 'Legion Slim 7i Gen 8', 'LNVY8B2L', 2024, 32, intel, 16, 1024, laptop, nvidia, 12).
computingPlatform('Lenovo', 'Legion 5i Gen 7', 'LNVZ7C3M', 2023, 32, intel, 14, 1024, laptop, nvidia, 8).
computingPlatform('Lenovo', 'LOQ 15 Gen 9', 'LNVW6D4N', 2023, 24, amd, 8, 1024, laptop, nvidia, 6).
computingPlatform('Lenovo', 'IdeaPad Gaming 3', 'LNVU1E5P', 2022, 16, amd, 6, 512, laptop, nvidia, 4).
computingPlatform('Lenovo', 'Legion Tab Gen 3', 'LNTG8A1X', 2025, 12, qualcomm, 8, 256, tablet, qualcomm, 0).
computingPlatform('Lenovo', 'Legion Tower 7i Gen 8', 'LNPC9A1Z', 2024, 64, intel, 24, 4096, desktop, nvidia, 24).

% Available Asus computing platforms
computingPlatform('ASUS', 'ROG Strix Scar 18', 'ASX9A1Q', 2025, 64, intel, 24, 4096, laptop, nvidia, 24).
computingPlatform('ASUS', 'ROG Zephyrus G16', 'ASY8B2L', 2025, 32, amd, 16, 2048, laptop, nvidia, 16).
computingPlatform('ASUS', 'ROG Flow X13', 'ASZ7C3M', 2024, 32, amd, 8, 1024, laptop, nvidia, 8).
computingPlatform('ASUS', 'TUF Gaming A15', 'AST6D4N', 2023, 16, amd, 8, 1024, laptop, nvidia, 6).
computingPlatform('ASUS', 'ROG Strix G15', 'ASU1E5P', 2022, 16, intel, 6, 512, laptop, nvidia, 4).
computingPlatform('ASUS', 'ROG Flow Z13', 'ASTAB01', 2025, 128, amd, 16, 1024, tablet, amd, 0).
computingPlatform('ASUS', 'ROG Flow Z13', 'ASTAB02', 2023, 32, intel, 14, 512, tablet, intel, 0).

% Available 'RedMagic' computing platforms
computingPlatform('RedMagic', 'Titan 16 Pro', 'RDMX9A1Q', 2025, 32, intel, 24, 2048, laptop, nvidia, 8).
computingPlatform('RedMagic', 'Astra Gaming Tablet', 'RDMTAB01', 2025, 24, qualcomm, 8, 1024, tablet, qualcomm, 0).
computingPlatform('RedMagic', 'NOVA Gaming Tablet', 'RDMTAB02', 2025, 16, qualcomm, 8, 512, tablet, qualcomm, 0).

% Available MSI computing platforms
computingPlatform('MSI', 'Titan 18 HX', 'MSIX9A1Q', 2025, 128, intel, 24, 4096, laptop, nvidia, 24).
computingPlatform('MSI', 'Raider GE78 HX', 'MSIY8B2L', 2025, 64, intel, 24, 2048, laptop, nvidia, 16).
computingPlatform('MSI', 'Stealth 18 AI Studio', 'MSIZ7C3M', 2025, 64, intel, 24, 2048, laptop, nvidia, 16).
computingPlatform('MSI', 'Vector GP78 HX', 'MSIW6D4N', 2024, 64, intel, 20, 2048, laptop, nvidia, 12).
computingPlatform('MSI', 'Crosshair 17 HX', 'MSIU1E5P', 2024, 32, amd, 16, 1024, laptop, nvidia, 12).
computingPlatform('MSI', 'Pulse GL76', 'MSIT3F6Q', 2023, 32, intel, 16, 1024, laptop, nvidia, 8).
computingPlatform('MSI', 'Sword 15 HX', 'MSIS4G7R', 2023, 16, intel, 8, 512, laptop, nvidia, 6).
computingPlatform('MSI', 'Cyborg 15 AI', 'MSIR5H8S', 2022, 16, intel, 8, 512, laptop, nvidia, 4).
computingPlatform('MSI', 'MEG Vision X AI', 'MSIDC01', 2025, 64, intel, 24, 2048, desktop, nvidia, 32).
computingPlatform('MSI', 'Infinite RS 14th Gen', 'MSIDC02', 2024, 32, intel, 20, 1024, desktop, nvidia, 16).

% Available Razer computing platforms
computingPlatform('Razer', 'Blade 18 Pro', 'RZRX9A1Q', 2025, 128, intel, 24, 4096, laptop, nvidia, 24).
computingPlatform('Razer', 'Blade 16 OLED', 'RZRY8B2L', 2025, 64, amd, 16, 2048, laptop, nvidia, 16).
computingPlatform('Razer', 'Blade 15 Advanced', 'RZRZ7C3M', 2024, 32, intel, 16, 1024, laptop, nvidia, 12).
computingPlatform('Razer', 'Blade 14 Ryzen', 'RZRW6D4N', 2023, 32, amd, 8, 1024, laptop, nvidia, 8).
computingPlatform('Razer', 'Blade Stealth 13', 'RZRU1E5P', 2022, 16, intel, 4, 512, laptop, intel, 0).
computingPlatform('Razer', 'Tomahawk Elite', 'RZRDC01', 2025, 64, intel, 24, 4096, desktop, nvidia, 32).
computingPlatform('Razer', 'Tomahawk ATX', 'RZRDC02', 2024, 32, amd, 16, 2048, desktop, nvidia, 16).

% Available Dell computing platforms
computingPlatform('Dell', 'Alienware m18 R2', 'DLX9A1Q', 2025, 64, intel, 24, 12288, laptop, nvidia, 24).
computingPlatform('Dell', 'Alienware x16 R2', 'DLY8B2L', 2025, 64, intel, 24, 4096, laptop, nvidia, 24).
computingPlatform('Dell', 'Alienware x14 R2', 'DLZ7C3M', 2024, 32, intel, 16, 2048, laptop, nvidia, 16).
computingPlatform('Dell', 'G16 7630', 'DLW6D4N', 2024, 32, intel, 16, 2048, laptop, nvidia, 12).
computingPlatform('Dell', 'Alienware m15 R7', 'DLT3F6Q', 2023, 32, amd, 8, 1024, laptop, nvidia, 8).
computingPlatform('Dell', 'Alienware Area-51', 'DLDC01', 2025, 64, intel, 24, 4096, desktop, nvidia, 32).
computingPlatform('Dell', 'Alienware Aurora R16', 'DLDC02', 2024, 64, intel, 24, 8192, desktop, nvidia, 24).

