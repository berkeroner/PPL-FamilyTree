% Facts for storing person information
% person(Name, Surname, Gender, BirthYear, DeathYear, Father, Mother)
:- dynamic person/7.
:- dynamic marriage/2.
:- dynamic child/2.

% Helper predicates
full_name(Name, Surname, FullName) :-
    atom_concat(Name, ' ', Temp),
    atom_concat(Temp, Surname, FullName).

is_alive(FullName) :-
    person(Name, Surname, _, _, DeathYear, _, _),
    full_name(Name, Surname, FullName),
    (var(DeathYear) ; DeathYear = none).

age(FullName, Age) :-
    person(Name, Surname, _, BirthYear, DeathYear, _, _),
    full_name(Name, Surname, FullName),
    get_time(TimeStamp),
    stamp_date_time(TimeStamp, DateTime, local),
    date_time_value(year, DateTime, CurrentYear),
    (   (var(DeathYear) ; DeathYear = none)
    ->  EndYear = CurrentYear 
    ;   EndYear = DeathYear
    ),
    Age is EndYear - BirthYear.

% Add person predicate
add_person(Name, Surname, Gender, BirthYear, DeathYear, Father, Mother) :-
    full_name(Name, Surname, FullName),
    \+ person(Name, Surname, _, _, _, _, _),
    % Handle death year
    (   (DeathYear = none ; var(DeathYear))
    ->  ActualDeathYear = none
    ;   ActualDeathYear = DeathYear
    ),
    % Handle parent names
    (   Father = none
    ->  ActualFather = none
    ;   ActualFather = Father
    ),
    (   Mother = none  
    ->  ActualMother = none
    ;   ActualMother = Mother
    ),
    assertz(person(Name, Surname, Gender, BirthYear, ActualDeathYear, ActualFather, ActualMother)),
    (   ActualFather \= none,
        person(FName, FSurname, _, _, _, _, _),
        full_name(FName, FSurname, ActualFather)
    ->  assertz(child(ActualFather, FullName))
    ;   true
    ),
    (   ActualMother \= none,
        person(MName, MSurname, _, _, _, _, _),
        full_name(MName, MSurname, ActualMother)
    ->  assertz(child(ActualMother, FullName))
    ;   true
    ).

% Update birth year
update_birth(FullName, NewBirthYear) :-
    person(Name, Surname, Gender, _, DeathYear, Father, Mother),
    full_name(Name, Surname, FullName),
    retract(person(Name, Surname, Gender, _, DeathYear, Father, Mother)),
    assertz(person(Name, Surname, Gender, NewBirthYear, DeathYear, Father, Mother)).

% Update death year
update_death(FullName, NewDeathYear) :-
    person(Name, Surname, Gender, BirthYear, _, Father, Mother),
    full_name(Name, Surname, FullName),
    retract(person(Name, Surname, Gender, BirthYear, _, Father, Mother)),
    (   (NewDeathYear = none ; NewDeathYear = 0)
    ->  DeathValue = none
    ;   (   NewDeathYear < BirthYear
        ->  write('Death year cannot be earlier than birth year.'), nl,
            DeathValue = none
        ;   DeathValue = NewDeathYear
        )
    ),
    assertz(person(Name, Surname, Gender, BirthYear, DeathValue, Father, Mother)).

% Calculate level in family tree
level(FullName, Level) :-
    person(Name, Surname, _, _, _, Father, Mother),
    full_name(Name, Surname, FullName),
    get_parent_level(Father, FatherLevel),
    get_parent_level(Mother, MotherLevel),
    max_list([FatherLevel, MotherLevel, -1], MaxParentLevel),
    Level is MaxParentLevel + 1.

% Helper to get parents level safely
get_parent_level(none, -1) :- !.
get_parent_level(ParentFullName, Level) :-
    person(PName, PSurname, _, _, _, _, _),
    full_name(PName, PSurname, ParentFullName),
    level(ParentFullName, Level), !.
get_parent_level(_, -1).  % Parent not found in database


% Print person information
print_info(FullName) :-
    age(FullName, Age),
    level(FullName, Level),
    findall(Child, child(FullName, Child), Children),
    length(Children, ChildCount),
    format('Age: ~w~n', [Age]),
    format('Level: ~w~n', [Level]),
    format('Total child: ~w~n', [ChildCount]),
    (is_alive(FullName) -> write('Alive') ; write('Dead')), nl.

is_married(Person) :-
    marriage(Person, _);
    marriage(_, Person).

% Marriage predicate
marry(Name1, Name2) :-
    person(N1, S1, G1, _, _, _, _),
    person(N2, S2, G2, _, _, _, _),
    full_name(N1, S1, Name1),
    full_name(N2, S2, Name2),
    (   G1 = G2
    ->  write('Same-sex marriage!.'), nl
    ;   (   age(Name1, Age1), age(Name2, Age2),
            (Age1 < 18 ; Age2 < 18)
        ->  write('Under 18 age marriage!'), nl
        ;   (   are_close_relatives(Name1, Name2)
            ->  write('Invalid marriage due to close relationship.'), nl
            ;   (   is_married(Name1)
                ->  write(Name1), write(' is already married!'), nl
                ;   (   is_married(Name2)
                    ->  write(Name2), write(' is already married!'), nl
                ;   assertz(marriage(Name1, Name2)),
                    write('Marriage registered.'), nl
            )
            )
        )
        )
    ).
is_uncle_by_father(Uncle, Person) :-
    person(N, S, _, _, _, Father, _),
    full_name(N, S, Person),
    are_siblings(Uncle, Father),
    person(NU, SU, m, _, _, _, _),
    full_name(NU, SU, Uncle).   
is_uncle_by_mother(Uncle, Person) :-
    person(N, S, _, _, _, _, Mother),
    full_name(N, S, Person),
    are_siblings(Uncle, Mother),
    person(NU, SU, m, _, _, _, _),
    full_name(NU, SU, Uncle).

is_aunt_by_father(Aunt, Person) :-
    person(N, S, _, _, _, Father, _),
    full_name(N, S, Person),
    are_siblings(Aunt, Father),
    person(NA, SA, f, _, _, _, _),
    full_name(NA, SA, Aunt). 

is_aunt_by_mother(Aunt, Person) :-
    person(N, S, _, _, _, _, Mother),
    full_name(N, S, Person),
    are_siblings(Aunt, Mother),
    person(NA, SA, f, _, _, _, _),
    full_name(NA, SA, Aunt).   

is_parent_of(Parent, Child) :-
    person(NC, SC, _, _, _, Father, Mother),
    full_name(NC, SC, Child),
    (Parent = Father ; Parent = Mother),
    Parent \= none.

is_grandparent_of(Grandparent, Child) :-
    person(NC, SC, _, _, _, Father, Mother),
    full_name(NC, SC, Child),
    (   person(NF, SF, _, _, _, GF1, GM1),
        full_name(NF, SF, Father),
        (Grandparent = GF1; Grandparent = GM1)
    ;
        person(NM, SM, _, _, _, GF2, GM2),
        full_name(NM, SM, Mother),
        (Grandparent = GF2; Grandparent = GM2)
    ).    
% Check if two people are siblings
are_siblings(Name1, Name2) :-
    person(N1, S1, _, _, _, Father, Mother),
    person(N2, S2, _, _, _, Father, Mother),
    full_name(N1, S1, Name1),
    full_name(N2, S2, Name2),
    Name1 \= Name2,
    Father \= none,
    Mother \= none.

% Check if two people are close relatives
are_close_relatives(Name1, Name2) :-
    (   are_siblings(Name1, Name2)
    ;   person(N1, S1, _, _, _, Name2, _), full_name(N1, S1, Name1)
    ;   person(N1, S1, _, _, _, _, Name2), full_name(N1, S1, Name1)
    ;   person(N2, S2, _, _, _, Name1, _), full_name(N2, S2, Name2)
    ;   person(N2, S2, _, _, _, _, Name1), full_name(N2, S2, Name2)
    ;   is_grandparent_of(Name1, Name2)
    ;   is_grandparent_of(Name2, Name1)
    ;   is_uncle_by_father(Name1, Name2)
    ;   is_uncle_by_father(Name2, Name1)
    ;   is_uncle_by_mother(Name1, Name2)
    ;   is_uncle_by_mother(Name2, Name1)
    ;   is_aunt_by_father(Name1, Name2)
    ;   is_aunt_by_father(Name2, Name1)
    ;   is_aunt_by_mother(Name1, Name2)
    ;   is_aunt_by_mother(Name2, Name1)
    ).
    
% Print family tree
print_tree :-
    findall(Level-FullName, 
            (person(N, S, _, _, _, _, _), full_name(N, S, FullName), level(FullName, Level)), 
            LevelPersons),
    include(should_include_person, LevelPersons, Filtered),
    adjust_spouse_levels(Filtered, Adjusted),
    keysort(Adjusted, Sorted),
    group_pairs_by_key(Sorted, Grouped),
    print_levels(Grouped).


adjust_spouse_levels(LevelPairs, Adjusted) :-
    adjust_spouse_levels(LevelPairs, LevelPairs, Adjusted).

adjust_spouse_levels([], _, []).
adjust_spouse_levels([Level1-Name|Rest], All, [FinalLevel-Name|AdjustedRest]) :-
    (   (marriage(Name, Spouse) ; marriage(Spouse, Name)),
        member(Level2-Spouse, All)
    ->  FinalLevel is max(Level1, Level2)
    ;   FinalLevel = Level1
    ),
    adjust_spouse_levels(Rest, All, AdjustedRest).


print_levels([]).
print_levels([Level-Names|Rest]) :-
    format('---LEVEL ~w---~n', [Level]),
    print_level_names(Names, []),
    print_levels(Rest).

print_level_names([], _).
print_level_names([Name|Rest], Printed) :-
    (   member(Name, Printed)
    ->  print_level_names(Rest, Printed)
    ;   (   (marriage(Name, Spouse) ; marriage(Spouse, Name)), 
            member(Spouse, [Name|Rest]), 
            Spouse \= Name
        ->  format('~w - ~w~n', [Name, Spouse]),
            append(Printed, [Name, Spouse], NewPrinted)
        ;   format('~w~n', [Name]),
            append(Printed, [Name], NewPrinted)
        ),
        print_level_names(Rest, NewPrinted)
    ).

should_include_person(_-FullName) :-
    % Kişinin anne veya babası soy ağacında kayıtlıysa göster
    person(N, S, _, _, _, Father, Mother),
    full_name(N, S, FullName),
    (   (Father \= none, person(FN, FS, _, _, _, _, _), full_name(FN, FS, Father))
    ;   (Mother \= none, person(MN, MS, _, _, _, _, _), full_name(MN, MS, Mother))
    ;   (marriage(FullName, _) ; marriage(_, FullName))  % Veya evliyse göster
    ).


% Relation finding predicates
find_relation(Name1, Name2, Relation) :-
    person(N1, S1, G1, _, _, F1, M1),
    person(N2, S2, G2, _, _, F2, M2),
    full_name(N1, S1, Name1),
    full_name(N2, S2, Name2),
    (   Name1 = F2
    ->  Relation = 'Baba'
    ;   Name1 = M2
    ->  Relation = 'Anne'
    ;   Name2 = F1
    ->  (G1 = m -> Relation = 'Ogul' ; Relation = 'Kiz')
    ;   Name2 = M1
    ->  (G1 = m -> Relation = 'Ogul' ; Relation = 'Kiz')
    ;   are_siblings(Name1, Name2)
    ->  (   age(Name1, Age1), age(Name2, Age2),
            (   Age1 > Age2
            ->  (G1 = m -> Relation = 'Abi' ; Relation = 'Abla')
            ;   Relation = 'Kardes'
            )
        )
    ;   (   (F2 \= none, are_siblings(Name1, F2)) ; (M2 \= none, are_siblings(Name1, M2))
        ->  (G1 = m -> Relation = 'Dayi' ; Relation = 'Teyze')
        ;   (   (F1 \= none, are_siblings(Name2, F1)) ; (M1 \= none, are_siblings(Name2, M1))
            ->  Relation = 'Yeğen'
            ;   (   (F1 \= none, F2 \= none, are_siblings(F1, F2)) ; (M1 \= none, M2 \= none, are_siblings(M1, M2))
                ->  Relation = 'Kuzen'
                ;   (   marriage(Name1, Name2) ; marriage(Name2, Name1)
                    ->  Relation = 'Eş'
                    ;   find_marriage_relation(Name1, Name2, G1, G2, Relation)
                    )
                )
            )
        )
    ).

find_marriage_relation(Name1, Name2, _, _, Relation) :-
    (   marriage(Name1, Spouse1) ; marriage(Spouse1, Name1)
    ),
    Spouse1 = Name2,
    Relation = 'Eş'.

find_marriage_relation(Name1, Name2, _, G2, Relation) :-
    (   marriage(Name1, Spouse1) ; marriage(Spouse1, Name1)
    ),
    Spouse1 \= Name2,
    person(SN, SS, _, _, _, SF, SM),
    full_name(SN, SS, Spouse1),
    (   are_siblings(Spouse1, Name2)
    ->  (G2 = f -> Relation = 'Baldiz' ; Relation = 'Kayinbirader')
    ;   Name2 = SM
    ->  Relation = 'Kayinvalide'
    ;   Name2 = SF
    ->  Relation = 'Kayinpeder'
    ;   Relation = 'No direct relationship found.'
    ).

% CLI Interface
run_cli :-
    repeat,
    nl,
    write('1-) Ask relation'), nl,
    write('2-) Add/Update person'), nl,
    write('3-) Get information of any person'), nl,
    write('4-) Print the family tree'), nl,
    write('5-) Add marriage'), nl,
    write('6-) Terminate the program'), nl,
    nl,
    write('Please choose an operation!'), nl,
    write('|: '),
    read_line_to_string(user_input, Choice),
    atom_string(ChoiceAtom, Choice),
    process_choice(ChoiceAtom),
    ChoiceAtom = '6',
    !.

process_choice('1') :-
    write('please type first person name and surname:'), nl,
    write('|: '),
    read_line_to_string(user_input, Name1),
    write('please type second person name and surname:'), nl,
    write('|: '),
    read_line_to_string(user_input, Name2),
    atom_string(Name1Atom, Name1),
    atom_string(Name2Atom, Name2),
    (   person(N1, S1, _, _, _, _, _), full_name(N1, S1, Name1Atom),
        person(N2, S2, _, _, _, _, _), full_name(N2, S2, Name2Atom)
    ->  (   find_relation(Name1Atom, Name2Atom, Relation)
        ->  write(Relation), nl
        ;   write('No direct relationship found.'), nl
        )
    ;   write('One or both persons not found.'), nl
    ).

process_choice('2') :-
    write('1-) Add person'), nl,
    write('2-) Update person'), nl,
    write('Please choose an operation!'), nl,
    write('|: '),
    read_line_to_string(user_input, SubChoice),
    atom_string(SubChoiceAtom, SubChoice),
    process_person_operation(SubChoiceAtom).

process_choice('3') :-
    write('please type the person name and surname:'), nl,
    write('|: '),
    read_line_to_string(user_input, Name),
    atom_string(NameAtom, Name),
    (   person(N, S, _, _, _, _, _), full_name(N, S, NameAtom)
    ->  print_info(NameAtom)
    ;   write('Person not found.'), nl
    ).

process_choice('4') :-
    print_tree.

process_choice('5') :-
    write('name of first person :'), nl,
    write('|: '),
    read_line_to_string(user_input, N1),
    write('name of second person :'), nl,
    write('|: '),
    read_line_to_string(user_input, N2),
    atom_string(N1Atom, N1),
    atom_string(N2Atom, N2),
    (   person(Name1, Surname1, _, _, _, _, _), full_name(Name1, Surname1, N1Atom),
        person(Name2, Surname2, _, _, _, _, _), full_name(Name2, Surname2, N2Atom)
    ->  marry(N1Atom, N2Atom)
    ;   write('One or both people not found.'), nl
    ).

process_choice('6') :-
    write('Goodbye!'), nl.

process_choice(_) :-
    write('Invalid choice.'), nl.

process_person_operation('1') :-
    write('please type the father name and surname:'), nl,
    write('|: '),
    read_line_to_string(user_input, Father),
    write('please type the mother name and surname:'), nl,
    write('|: '),
    read_line_to_string(user_input, Mother),
    write('please type the child name and surname:'), nl,
    write('|: '),
    read_line_to_string(user_input, Child),
    write('please type the birthdate of the child:'), nl,
    write('|: '),
    read_line_to_string(user_input, BirthStr),
    atom_string(BirthAtom, BirthStr),
    atom_number(BirthAtom, Birth),
    write('please type the death date of the child:'), nl,
    write('|: '),
    read_line_to_string(user_input, DeathStr),
    atom_string(DeathAtom, DeathStr),
    (   DeathAtom = 'none'
    ->  Death = none
    ;   atom_number(DeathAtom, Death)
    ),
    write('please type the child person gender:'), nl,
    write('|: '),
    read_line_to_string(user_input, GenderStr),
    atom_string(Gender, GenderStr),
    split_string(Child, ' ', ' ', [NameStr, SurnameStr]),
    atom_string(Name, NameStr),
    atom_string(Surname, SurnameStr),
    atom_string(FatherAtom, Father),
    atom_string(MotherAtom, Mother),
    add_person(Name, Surname, Gender, Birth, Death, FatherAtom, MotherAtom),
    write('Child added.'), nl.

process_person_operation('2') :-
    write('Full name: '),
    read_line_to_string(user_input, Name),
    atom_string(NameAtom, Name),
    (   person(N, S, _, _, _, _, _), full_name(N, S, NameAtom)
    ->  write('1. Update the birth year of someone.'), nl,
        write('2. Update the death year of someone.'), nl,
        write('0. Cancel.'), nl,
        write('Enter your choice: '),
        read_line_to_string(user_input, What),
        atom_string(WhatAtom, What),
        process_update_operation(NameAtom, WhatAtom)
    ;   write('Person not found.'), nl
    ).

process_update_operation(NameAtom, '1') :-
    write('Enter new birth year: '),
    read_line_to_string(user_input, YearStr),
    atom_string(YearAtom, YearStr),
    atom_number(YearAtom, NewYear),
    update_birth(NameAtom, NewYear).

process_update_operation(NameAtom, '2') :-
    write('Enter new death year: '),
    read_line_to_string(user_input, YearStr),
    atom_string(YearAtom, YearStr),
    atom_number(YearAtom, NewYear),
    update_death(NameAtom, NewYear).

process_update_operation(_, '0').

% Start the program
start :-
    run_cli.