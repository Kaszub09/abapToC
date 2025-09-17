# abapToC
ABAP Transport of Copies

## Features

1. New Transaction *ZTOC* which allows for easy creation/release/import of Transport of Copies

<img width="1015" height="593" alt="obraz" src="https://github.com/user-attachments/assets/b9ba4d10-0db1-4b93-9d67-ff1f9017a716" />

![report](https://github.com/Kaszub09/abapToC/assets/34368953/9942d528-7b71-4db8-bcf1-82906ed1aa90)

2. Create, release and import Transport of Copies with one click:
   
![obraz](https://github.com/Kaszub09/abapToC/assets/34368953/7cc59ec7-5fe2-439b-8771-9051b78d0197)

## Installation

1. Use abapGit https://github.com/abapGit/abapGit 
2. In order for import to work, you must:
   1. Import/Transport project to target system
   2. Create connection for target system in SM59 - for each possible target in transport (e.g. 'SYSTEM', 'SYSTEM.MANDANT' ) create connection with exact same name - will be used to call RFC which unpacks transport. So ZZZ.999 for system ZZZ and mandant 999, or just ZZZ if you don't specify mandant in transports.
   3. [Optional] Set developer system as trusted (via transaction SMT1) at target system. Set Trust Relationship to yes in connections and use Current User. This way you won't have to log into target system everytime you wan't to import transport - you will be automatically logged with current user.

## Notes
1. Written in ABAP 7.50

## Changelog

- v1.1 - allow user to choose between 3 types of descriptions - ToC + original request number, original request description, or custom / numbered description
- v1.2 - allow user mass action on all selected ToCs
- v1.2.1 - add STMS button
- v1.3 - retry importing ToC for a specified amount of time if not yet visible in target system (apparently, release funciton isn't fully synchronous?)
- v1.4 - add support for sub transport, minor enhancements
- 11.09.2025 - add support for selecting different target system (thanks for idea [jrgkraus](https://github.com/jrgkraus)
- 17.09.2025 - add support for transport groups

