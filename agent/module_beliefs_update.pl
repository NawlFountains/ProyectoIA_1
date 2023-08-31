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


update_beliefs(Perc):- forall(Rel,Perc),update(Rel)

update(Perc):- Perc = time(_), retract(time(_)), assert(Perc).

update(Perc):- Perc = direction(_), retract(direction(_)), assert(Perc).

update(Perc):- Perc = at(IdNodo,TipoEntidad,IdEntidad), retract(IdNodo,_,_), assert(Perc).

update(Perc):- Perc = node(Id, PosX, PosY, Costo, Conexiones), retract(node(Id, PosX, PosY, _, _)), assert(Perc).
