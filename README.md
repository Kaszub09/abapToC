# abapToC
ABAP Transport of Copies

## Features

1. New Transaction *ZTOC* which allows for easy creation/release/import of Transport of Copies

![obraz](https://github.com/Kaszub09/abapToC/assets/34368953/5d88049f-759c-40e7-addb-2d443c4af5be)
![report](https://github.com/Kaszub09/abapToC/assets/34368953/9942d528-7b71-4db8-bcf1-82906ed1aa90)

2. Create, release and import Transport of Copies with one click:
![obraz](https://github.com/Kaszub09/abapToC/assets/34368953/7cc59ec7-5fe2-439b-8771-9051b78d0197)

## Installation

1. Use [abapGit](ttps://github.com/abapGit/abapGit)
2. In order for import to work, you must:
   1. Import/Transport project to target system
   2. Create connection for target system in SM59 - for each possible target in transport (e.g. 'SYSTEM', 'SYSTEM.MANDANT' ) create connection with exact same name - will be used to call RFC which unpacks transport. So ZZZ.999 for system ZZZ and mandant 999, or just ZZZ if you don't specify mandant in transports.
   3. [Optional] Set developer system as trusted (via transaction SMT1) at target system. Set Trust Relationship to yes in connections and use Current User. This way you won't have to log into target system everytime you wan't to import transport - you will be automatically logged with current user.
