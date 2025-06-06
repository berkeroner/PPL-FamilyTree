# PPL-FamilyTree

# Family Tree CLI in Prolog

This is a command-line based family tree application implemented in **Prolog**.  
The system allows you to add and update people, define parent-child and marriage relationships, calculate levels in the family tree, and find the relationship between any two people.

## Features

- Add people with name, surname, gender, birth and death years, and parents.
- Update birth or death year of any person.
- Register marriages and prevent illegal ones (same gender, close relatives, or underage).
- Print a structured family tree grouped by generation levels.
- Query the relationship between two people (e.g., father, sibling, cousin, etc.).
- Prevent sibling, parent-child, and uncle/aunt-nephew/niece marriages.
- Track whether people are alive based on death year.

## Technologies

- Language: **Prolog**
- Interpreter: SWI-Prolog recommended

## Project Structure

```
.
├── main.pl      % Main Prolog source file
├── README.md           % This file
```

## How to Run

1. Install [SWI-Prolog](https://www.swi-prolog.org/Download.html) if not already installed.
2. Clone this repository:
   ```bash
   git clone https://github.com/berkeroner/PPL-FamilyTree.git
   cd your-repo-name
   ```
3. Start SWI-Prolog:
   ```bash
   swipl
   ```
4. Load the main file:
   ```prolog
   ?- [family_tree].
   ?- start.
   ```

## Menu Options

Once started, you will see the following CLI:

```
1-) Ask relation
2-) Add/Update person
3-) Get information of any person
4-) Print the family tree
5-) Add marriage
6-) Terminate the program
```

## Sample Inputs

Add a person:
```
please type the father name and surname:
|: none
please type the mother name and surname:
|: none
please type the child name and surname:
|: peter parker
please type the birthdate of the child:
|: 1988
please type the death date of the child:
|: none
please type the child person gender:
|: m
```

Register a marriage:
```
name of first person:
|: peter parker
name of second person:
|: mary jane
```

Print tree:
```
|: 4
```
