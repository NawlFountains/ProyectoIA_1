:- module(beliefs_update,
	  [
	    update_beliefs/1,
	    time/1,
	    node/5,
	    at/3,
		direction/1
	  ]).

:- dynamic time/1, node/5, at/3, direction/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TO-DO
%
% update_beliefs(+Perc)
%
% IMPORTANTE: Debe exportarse todo predicado dinámico (creencia)
% manipulado por la actualización de creencias, para que puedan ser
% consultado por el resto del código del agente.
%
% El parámetro Perc recibe una lista con el siguiente formato: [N1,...,Nk,A1,...Ap,Time,Dir]
% donde: 
% N1 a Nk son k elementos (k>0) de la forma node(Id, PosX, PosY, Costo, Conexiones),
% A1 a Ap son p elementos (p>0) de la forma at(IdNodo, TipoEntidad, IdEntidad),
% Time es el functor time(T), donde T es el tiempo actual (descendente) de la partida.
% Dir es el functor direction(D), donde D ∈ {w, s, a, d}.
%
% Este agente básico, al recibir una nueva percepcion olvida todo lo que tenía guardado en su estado interno
% Y almance la información de la percepción nueva.
%
% Pueden realizar todos los cambios de implementación que consideren necesarios.
% Esta implementación busca ser un marco para facilitar la resolución del proyecto.



update_beliefs([]).
update_beliefs(Perc):- findall(at(IdNodo,TipoEntidad,IdEntidad),member(at(IdNodo,TipoEntidad,IdEntidad),Perc),AtList),
					forall(member(Rel,Perc),
					update(Rel,AtList)). 
		       	     

% update(+Perc,+Atlist).
%
% update recibe una percepcion y una lista de objetos y dependiendo de que es la percepcion realiza distintas acciones
% update(N,AtList) y update(N,_) son los casos mas interesantes ya que son los que se encargan de revisar las creencias de los objetos que ve o deja de ver el agente
update(T,_):- T = time(_), 
	    retractall(time(_)),
	    assert(T).

update(D,_):- D = direction(_), 
	    retractall(direction(_)),
	    assert(D).

update(A,_):- A = at(IdNodo,TipoEntidad,_), 
	    TipoEntidad \= agente,
		retractall(at(IdNodo,_,_)),
	    assert(A).

update(A,_):- A = at(_,agente,_),
	    retractall(at(_,agente,_)),
	    assert(A).

update(N,AtList):- N = node(Id, PosX, PosY, Costo, Conexiones),
		member(at(Id,_,_),AtList),!,
	    retractall(node(Id,_,_,_,_)), 
	    assert(node(Id, PosX, PosY, Costo, Conexiones)).

update(N,_):- N = node(Id, PosX, PosY, Costo, Conexiones),
		retractall(at(Id,_,_)),
	    retractall(node(Id,_,_,_,_)), 
	    assert(node(Id, PosX, PosY, Costo, Conexiones)).



