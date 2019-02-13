%% functia principala ea introduce in baza de constinte nodul de unde pleca cu fromNode,
%% apeleaza : init_queue pentru a initializa coada, 
%% introducerea "functiile culoare", care verifica daca primul este de acea culoare, atomul: color
%% introducerea in baza de date unei relati nod - culoare : colorNode
%% apelarea a bfs pentru aflarea cai de lungime minima care verifica formula

getPath(From, To, Graph, Formula, Path) :-
    assert( fromNode(From) ),
    init_queue(From, Queue),
    color,
    color_node,
    bfs(Graph, To, Queue, Path, Formula).


%% functiile care spun daca un element apartine sau nu unei liste
contains([E|_], E).
contains([H|T], E):- 
    H \= E, 
    contains(T, E).

notContains(List, Element):-
    \+ contains(List, Element).

	
%% functia care retuneza ce culoare are nodul target
getColorNode([ [[Node, Color] | Tail], _], Target, TargetColor):-
    (Node == Target ->
        TargetColor = Color;
        getColorNode([Tail, _], Target, TargetColor)
    ).

%% ceaza o lista cu culoriile nodurilor din graf, fara sa se repecte o culoare
getColor( [ [], _ ],  []) :- !.
getColor( [ [ [_, Color] | T ], _], ListColor) :-
    getColor( [T, _], ListColorAux),
    (notContains(ListColorAux, Color) -> 
        ListColor = [Color | ListColorAux];
        ListColor = ListColorAux
    ) .

%% functia care primeste lista de culori si creaza functia corespunzatoare
%% care varifica daca primul nod are repectiva culoare
write_color([]) :- !.
write_color( [Head | Tail] ) :-
    assert( ( Head :- fromNode(To),
              input(Graph),
              getColorNode(Graph, To, NodeColor),
              Head == NodeColor) ), 
    write_color(Tail).
    
color :-
    input(Graph),
    getColor(Graph, ListColor),
    write_color(ListColor).

%% clauza care creaza/introduce in baza de data colorNode, relatia care spune
%% care este culoarea unui nod
write_color_node([ [], _] ) :- !. %% cand nu mai avem nimic de scris
write_color_node([ [[Node, Color] | Tail], _]) :-
    assert( colorNode(Node, Color) ),
    write_color_node([Tail, _]). 

color_node :-
    input(Graph),
    write_color_node(Graph).


%% functi ajutaoare pentru bfs, initializarea cozi, introduceri si care genereaza vecini nevizitati
%% in coada se vor afla path, apoi ele sunt inversata
%% exmplu daca avem o cale de la 1 la 2 si 1 la 3 in coada se vor afla [3, 1], [2, 1], prima este [2, 1]
%% este inversata pentru a lucra mai usor  

%% semanatura functiei : getOutNeighborsNotVisited(Graph, Node, Visited, ListOfOutNeighborsNotVisited)
%% clazua genereaza nodurile vecine ale lui N pentru care exista muchie de la N la ele (muchi de iesiere)
%% si care nu sunt in Visited
getOutNeighborsNotVisited([_, [] ], _, _, []) :- !.
getOutNeighborsNotVisited( [_ , [ [Surs, Dest] | T ] ], N, Visted, OutNeighbors ) :-
        ( (Surs == N, notContains(Visted, Dest))  ->
            getOutNeighborsNotVisited( [_, T], N, Visted, OutNeighborsAux), OutNeighbors = [ Dest | OutNeighborsAux];
            getOutNeighborsNotVisited( [_, T], N, Visted, OutNeighbors)        
        ).

%% functia care adauga fiecare elemntele din [H | T] intr-o lista de forma [H | List]
%% apoi lista rezulta este adaugat in lista Rerz
%% elemplu [H | T] = [2, 4, 5], List = [1]
%% rezulatul va fii Rerz = [ [2, 1], [4, 1], [5, 1] ]
add_to_list([], _, []).
add_to_list([H|T], List, Rerz) :-
    add_to_list(T, List, R1),
    Rerz = [ [H | List] | R1].


%% actualizaeza coda dupa cum umareza
%% ListNeighborsNotVisited = [2, 3, 4], OldPath = [1], OldQueue = [[]]
%% rezulatatul este NewQueue = [ [2, 1], [3, 1], [4, 1]]
update_queue(ListNeighborsNotVisited, OldPath, OldQueue, NewOueue):-
    add_to_list(ListNeighborsNotVisited, OldPath, NewPath),
    append(OldQueue, NewPath, NewOueue).

%% initializeza coada 
init_queue(Node, Queue):-
    Queue = [[Node]].

%% cluza care determina drumul minim, cum nu avem costuri pe muchi am folosit un bfs modificat
%% semnatura functie este : bfs(Graph, From, Queue, Path, Fomula)

bfs([_, _], TargetNode, [ [TargetNode] ], _, _ ) :- !.
    
bfs([_, E], TargetNode, [ [Head | Tail] | RestQueue], Path, Formula) :-
    (
	Head \= TargetNode ->
        (
            getOutNeighborsNotVisited([_, E], Head, [Head | Tail], OutNeighbors),
            update_queue(OutNeighbors, [Head | Tail], RestQueue, NewOueue)
        );

        (   
            reverse([TargetNode | Tail], TemporalPath),
            assert( tempPath(TemporalPath) ),
                Formula ->
                    Path = TemporalPath , NewOueue = [[TargetNode]], !;

                    retract(tempPath(TemporalPath)), NewOueue = RestQueue 
        )
    ),
    bfs([_, E], TargetNode, NewOueue, Path, Formula).


%% Implemarea fiecarei formule

%% implementare future
%% verifica daca exista un nod de culoare Color in lista 
check_future(_, []) :- fail.
check_future(Color, [Head| Tail]) :-
    colorNode(Head, Color);
    check_future(Color, Tail).

future(Color) :-
    tempPath(Path),
    check_future(Color, Path), !.


%% functile pentru verificarea daca calea temporara contine daor noduri de culore Color
check_global([], _) :- !.
check_global([Head | Tail], Color):-
    colorNode(Head, Color),
    check_global(Tail, Color).
    
global( Color ) :-
    tempPath(Path),
    check_global(Path, Color), !.

%% impelmentarea pentru until
check_until(_, _, []) :- !.
check_until(CheckColor, StopColor, [Head | Tail]) :-
    (colorNode(Head,CheckColor) ->
        check_until(CheckColor, StopColor, Tail);
        colorNode(Head, StopColor) ->
        !; %% daca este nodul de oprit, se opreste evaluare listei
        fail
    ).

until( CheckColor, StopColor ) :-
    tempPath(Path),
    check_until(CheckColor, StopColor, Path), !.

%% implentarea next 
next( Fomula ) :-
    fromNode(X),
    retract( fromNode(X) ),
    tempPath( [Head, Head2 | Tail] ),
    retract( tempPath( [Head, Head2 | Tail]) ),
    assert( tempPath([Head2 | Tail]) ),
    assert( fromNode(Head2) ),
    Fomula,
    retract( fromNode(Head2) ),
    retract( tempPath([Head2 | Tail]) ),
    assert( tempPath([Head, Head2 | Tail]) ),
    assert( fromNode(X) ).

or( Fomula1, Fomula2) :-
    Fomula1;
    Fomula2.

and( Fomula1, Fomula2) :- !,
    (Fomula1),!,
    (Fomula2),!.

not( Fomula ) :-
    \+ Fomula.

valid :- !.
