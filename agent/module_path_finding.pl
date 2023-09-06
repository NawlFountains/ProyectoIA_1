:- module(path_finding,
	  [
	    buscar_plan_desplazamiento/4,
		raiz/1,
		padre/2,
		esMeta/1
	  ]).

:- use_module(module_beliefs_update, [node/5, at/3]).

:- dynamic padre/2, raiz/1, esMeta/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% eliminarPrimero(+Lista, +Elemento)
%
% Elimina el primer elemento de la lista.
%
eliminarPrimero([], []).
eliminarPrimero([_|Xs], Xs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% seleccionar(+Nodo, +Frontera, +FronteraSinNodo)
%	
% Selecciona el primer nodo de la lista Frontera.
%	
seleccionar(Nodo, [Nodo|RestoLista], RestoLista).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% encontrarCamino(+Meta, -Camino)
%
% Encuentra un camino a un nodo Meta.
% Usa las relaciones padre(Hijo, Padre) que se van agregando a la base de conocimiento
% cuando se agregan nuevos vecinos a la nueva frontera, 
% en la busqueda de llegar de un nodo origen a uno destino.
%
encontrarCamino(Nodo, []):- raiz(Nodo), !.
encontrarCamino(Nodo, [P|Camino]):-
	padre(Nodo, P),
	encontrarCamino(P, Camino).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%		
%
% crearPlan(+Camino, -Plan)
%
% Crea plan de movimientos para un camino elegido.
% Para cada nodo de un camino, crea una lista de acciones de movimiento avanzar(IdNodo)
% donde IdNodo es un identificador de un nodo.
% Camino es una lista conteniendo identificadores de nodos.
%
crearPlan([], []).
crearPlan(Camino, Plan):-
	findall(avanzar(Nodo), member(Nodo, Camino), Plan).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% buscar_plan_desplazamiento(+Metas, -Plan, -Destino, -Costo)
% Agregar todas las metas como hechos esMeta(idNodoMeta)
% Si tiene al menos una meta, pone el nodo actual del agente como raiz del árbol de búsqueda
% y busca el camino desde la posición del agente a un meta
% usando A* (buscarEstrella/5)
%


buscar_plan_desplazamiento(Metas, Plan, Destino, Costo):-
	forall(member(Meta, Metas), assert(esMeta(Meta))),
	at(MyNode, agente, me),
	length(Metas, CantMetas),
	CantMetas > 0,
	!,
	retractall(raiz(_)),
	assert(raiz(MyNode)),
	buscarEstrella([[MyNode, 0]], Metas, Camino, Costo, Destino),
	crearPlan(Camino, Plan),
	!,
	retractall(padre(_,_)). % Si no lo quitamos sigue buscando distintas soluciones no validas
	
buscar_plan_desplazamiento(_, [], [], 0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% buscarEstrella(+Frontera, +Metas, ?Camino, ?Costo, ?Destino)
% 
% Busca el camino optimo desde la frontera hacia la meta mas cercana, utilizando la estrategia de busqueda A*.
%
	
buscarEstrella(Frontera, Metas, Camino, Costo, Destino):-
	buscar(Frontera, [], Metas, Destino),
	encontrarCamino(Destino, C),
	append([Destino], C, C2),	
	reverse(C2, C3),
	costoCamino(C3, Costo),
	eliminarPrimero(C3, Camino),
	retractall(esMeta(_)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% buscar(+Frontera, +Visitados, +Metas, -Destino)
% 
% Busca el camino optimo desde la frontera hacia la Meta, utilizando la estrategia de busqueda A*.
% No devuelve el camino como un parametro, sino que agrega las relaciones padre(Hijo, Padre)
% que permita luego encontrar el camino y su costo.
%
% Caso 1: Si el nodo es meta, termina la búsqueda.
% Caso 2: Si el nodo no es meta
% Selecciono el primer nodo de la frontera, 
% Genera los vecinos,
% Agregar nodo a visitados,
% Agregar vecinos a frontera, con los cuidados necesarios de A*
% y llama recursivmaente con la nueva frontera.
	
buscar(Frontera, _, _M, Nodo):-
	seleccionar([Nodo, _], Frontera, _),
	esMeta(Nodo),
	!.

buscar(Frontera, Visitados, Metas, MM):-
	seleccionar(Nodo, Frontera, FronteraSinNodo), % selecciona primer nodo de la frontera
	generarVecinos(Nodo, Vecinos), % genera los vecinos del nodo 
	agregarAVisitados(Nodo, Visitados, NuevosVisitados), % agrega el nodo a lista de visitados
	agregar(FronteraSinNodo, Vecinos, NuevaFrontera, NuevosVisitados, Nodo, Metas), % agrega vecinos a la frontera
	buscar(NuevaFrontera, NuevosVisitados, Metas, MM). % continua la busqueda con la nueva frontera
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
%
% generarVecinos(+Nodo,-Vecinos)
% % Dado un nodo se obtienes sus nodos vecinos, aquellos en conexiones, % y se les suma el coste de pasar por este nodo % generarVecinos(Nodo,Vecinos):- Aux = [], Nodo = [Id,CostoNodo], 
generarVecinos(Nodo,Vecinos):-
		Nodo = [Id,CostoNodo],
		node(Id,_,_,_,Conexiones),
		!,
		findall([IDVecino,CostoVecinoPasando], (member([IDVecino,CostoVecinoSolo],Conexiones), CostoVecinoPasando is CostoVecinoSolo + CostoNodo), Vecinos).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% agregar(+FronteraSinNodo,+Vecinos,-NuevaFrontera,+NuevosVisitados,+Nodo,+Metas)
%
% Dado una Frontera y un Nodo, este no pertenece a la frontera, los vecinos de 
% dicho nodo , metas a buscar y los nodos que ya se visitaron se agregan los nodos
% vecinos a la frontera, agregandolos a la relacion padre/2 y ordenandolos por costo

agregar(FronteraSinNodo, Vecinos, NuevaFrontera, NuevosVisitados, Nodo, Metas):-
	quitarNodosRepetidos(Vecinos,NuevosVisitados,VecinosNoVisitados),
	quitarNodosRepetidos(VecinosNoVisitados,FronteraSinNodo,VecinosNoFrontera),
	% Necesitamos establecer relacion padre e hijo
	Nodo = [P,CostoNodo],
	forall(member([N,_],VecinosNoFrontera), assert(padre(N,P))),
	obtenerMasCercano(P,Metas,MetaMasCercana),
	calcularCosto(VecinosNoFrontera,MetaMasCercana,VecinosNoFronteraConCosto),
	insertarListaOrdenada(VecinosNoFronteraConCosto,FronteraSinNodo,NuevaFrontera).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
% quitarNodosRepetidos(+NodosOrigen, +NodosNoRepetir, -NodosSinRepetir)
%
% Similar a como funciona el operador - en conjuntos, permanecen 
% los nodos que se encuentran en NodosOrigen cuya Id no se encuentre
% en algun nodo de NodosNoRepetir.
%
% No se puede hacer con un subtract porque deben ser completamente identicos
% los nodos y el costo puede que sea distinto.

quitarNodosRepetidos(NodosOrigen,NodosNoRepetir,NodosSinRepetir):-
	findall(NodoSoloEnOrigen, (NodoSoloEnOrigen = [ID,_], member(NodoSoloEnOrigen,NodosOrigen), not(member([ID, _], NodosNoRepetir))), NodosSinRepetir).

%
% agregarAVisitados(+Nodo, +Visitados, ?VisitadosConNodo)
%
% Agrega un nodo a la lista de visitados.
%
agregarAVisitados(Nodo, Visitados, [Nodo | Visitados]).

%
% calcularCosto(+Nodos,+Meta,-NodosConCosto)
%
% Dada una lista de Nodos y un conjunto de metas calcula el costo
% dada la funcion f(n) = coste(n) + heuristica(n).

calcularCosto(Nodos, Meta, NodosConCosto):- findall([Id,Costo],(member([Id,CostoActual],Nodos), calcularH(Id,Meta,CostoHeuristica), Costo is CostoActual + CostoHeuristica),NodosConCosto).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% costoCamino(+Lista, ?Costo)
%
% Calcula el costo del camino, 
% como la sumatoria de los costos de los nodos que forma el camino.
% Lista es una lista conteniendo identificadores de nodos, representando el camino.
%
costoCamino([], 0).

costoCamino([X|Xs], R):-
	node(X, _, _, CostoNodo, _),
	costoCamino(Xs, CostoResto),
	R is CostoNodo + CostoResto.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% calcularH(+Nodo, ?Resultado, +Meta)
%
% Calcula el valor de la heurística para el nodo Nodo a una Meta.
% La heurística es la distancia euclidea.
%
calcularH(Nodo, Meta, Resultado):-
	node(Meta, X2, Y2, _, _),
	node(Nodo, X1, Y1, _, _),
	distance([X1, Y1], [X2, Y2], Resultado).

distance([X1, Y1], [X2, Y2], Distance):-
	DX is X2 - X1,
	DY is Y2 - Y1,
	Distance is sqrt(DX^2 + DY^2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%obtenerMasCercano(+Node,+ListaMetas,-MetaMasCernada)
%
%Obtiene la meta mas cercana de una lista de metas dado un nodo.
%
obtenerMasCercano(Node,[Meta],Meta) :- !.
obtenerMasCercano(Node,[Meta|ListaMetas],MetaMasCercana):- 
	obtenerMasCercano(Node,ListaMetas,MetaMasCercanaAux),
	masCerca(Node,Meta,MetaMasCercanaAux,MetaMasCercana).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%masCerca(+Node,+MetaA,+MetaB,-MetaMasCerca)
%
%Dada dos Metas define cual es la mas cercana a Node segun la distancia euclidia.

masCerca(Node,MetaA,MetaB,MetaA):- calcularH(Node,MetaA,HA),calcularH(Node,MetaB,HB),HA=<HB, !.
masCerca(Node,MetaA,MetaB,MetaB).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%insertarListaOrdenada(+ListaAinsertar,+ListaBase,-ListaInsertadaOrdenada)
%
%Dada una lista de nodos a insertar inserta cada nodo de forma ordenada en la lista base y lo retorna en la ListaInsertadaOrdena

insertarListaOrdenada([],Lista,Lista).
insertarListaOrdenada([Nodo|RestoLista],Lista,Resultado):- insertarOrdenado(Nodo,Lista,ListaOrdenada),insertarListaOrdenada(RestoLista,ListaOrdenada,Resultado).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%insertarOrdenado(+Nodo,+Lista,-ListaConNodoInsertado)
% 
%Dado un nodo y una lista inserta el nodo de forma ordenada en la lista.
insertarOrdenado(Nodo,[],[Nodo]).
insertarOrdenado(Nodo, [PrimerNodo|RestoLista], [Nodo, PrimerNodo|RestoLista]):-Nodo = [_, Costo],
										PrimerNodo = [_, CostoPrimerNodo],
										Costo =< CostoPrimerNodo.
										
insertarOrdenado(Nodo,[PrimerNodo|RestoLista],[PrimerNodo|Resultado]):- Nodo = [_,Costo],
									PrimerNodo = [_,CostoPrimerNodo],
									Costo > CostoPrimerNodo,
									insertarOrdenado(Nodo,RestoLista,Resultado).								
