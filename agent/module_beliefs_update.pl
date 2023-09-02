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
update_beliefs(Perc):- forall(member(Rel,Perc),update(Rel)). 
		       	     

update(T):- T = time(_), 
	    retractall(time(_)),
	    assert(T).

update(D):- D = direction(_), 
	    retractall(direction(_)),
	    assert(D).

update(A):- A = at(IdNodo,TipoEntidad,IdEntidad), 
	    TipoEntidad \= agente,
	    assert(A).
update(A):- A = at(IdNodo,agente,IdEntidad),
	    retractall(at(_,agente,_)),
	    assert(A).

update(N):- N = node(Id, PosX, PosY, Costo, Conexiones), 
	    retractall(at(Id,_,_)), 
	    retractall(node(Id,_,_,_)), 
	    assert(node(Id, PosX, PosY, Costo, Conexiones)).

