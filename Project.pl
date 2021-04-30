% Gonçalo Nunes 199229
% Projeto de LP 2020/2021
% Solucionador de Puzzles Kakuro

:- [codigo_comum, puzzles_publicos].

% -----------------------------------------------------------------------
%                   Combinacoes_soma(N, Els, Soma, Combs)
% combinacoes_soma(N, Els, Soma, Combs), em que N eh um inteiro, Els eh
% uma lista de inteiros, e Soma eh um inteiro, significa que Combs eh a
% lista ordenada cujos elementos sao as combinacoes N a N, dos
% elementos de Els cuja soma e Soma.
% -----------------------------------------------------------------------
combinacoes_soma(N, Els, Soma, Combs) :-
     findall(X,
             (combinacao(N, Els, X), sum_list(X, Sum),
              Sum =:= Soma), Combs).


% -----------------------------------------------------------------------
%                  permutacoes_soma(N, Els, Soma, Perms)
% permutacoes_soma(N, Els, Soma, Perms), em que N eh um inteiro, Els eh
% uma lista de inteiros, e Soma eh um inteiro, significa que Perms eh a
% lista ordenada cujos elementos sao as permutações das combinações N a
% N, dos elementos de Els cuja soma eh Soma.
% -----------------------------------------------------------------------
permutacoes_soma(N, Els, Soma, Perms) :-
     combinacoes_soma(N, Els, Soma, Combs),
     findall(X, (member(Y, Combs), permutation(Y, X)), Permutado),
     sort(Permutado,Perms).

% -----------------------------------------------------------------------
%                    espaco_fila(Fila, Esp, H_V)
% espaco_fila(Fila, Esp, H_V), em que Fila eh uma fila (linha ou coluna)
% de um puzzle e H_V eh um dos atomos h ou v, conforme se trate de uma
% fila horizontal ou vertical, respectivamente, significa que Esp eh um
% espaco de Fila, tal como descrito na Seccao 2.1, no passo 1.
% -----------------------------------------------------------------------
espaco_fila(Fila, Esp, H_V) :-
     espaco_fila_aux(Fila, Esp, H_V, _, _, _).
%     member(Esp, Conjunto).

% So mexer na soma quando chego a um dos casos terminais?

% Chegou ao fim da fila
%espaco_fila_aux([], Esp, _, Soma, Vars, Conjunto) :-
%     faz_espaco(Soma, Vars, Esp_Atual),
%     append(Conjunto, [Esp_Atual], Resultado),
%     !,
%     member(Esp, Resultado).

% Se ainda nao houve celulas livres
espaco_fila_aux([P | R], Esp, H_V, _, Vars, Conjunto) :-
     is_list(P),
     length(Vars, N),
     N =:= 0,
     valor_soma(P, H_V, Soma),
     espaco_fila_aux(R, Esp, H_V, Soma, Vars, Conjunto).


% Se for uma celula livre, a "direita" de uma restricao
espaco_fila_aux([P | R], Esp, H_V, Soma, Vars, Conjunto) :-
     var(P),
%     nonvar(Soma),
     append(Vars, [P], Novo_Vars),
     espaco_fila_aux(R, Esp, H_V, Soma, Novo_Vars, Conjunto).


% Fim do primeiro espaco dentro da fila
espaco_fila_aux([P | R], Esp, H_V, Soma, Vars, Conjunto) :-
     is_list(P),
     length(Vars, N),
     N =\= 0,
     faz_espaco(Soma, Vars, Esp_Atual),
     primeiro_espaco(Conjunto),
     append([Esp_Atual], Conjunto, Atualizado),
     espaco_fila_aux(R, Esp, H_V, _, _, Atualizado).

espaco_fila_aux([], Esp, _, Soma, Vars, Conjunto) :-
     faz_espaco(Soma, Vars, Esp_Atual),
     append(Conjunto, [Esp_Atual], Resultado),
     member(Esp, Resultado).


copia_lista([], []).

copia_lista([P | R], [P | R2]) :-
     copia_lista(R, R2).


% Fila horizontal
valor_soma(Lista, H_V, Soma) :-
    H_V == h,
    nth0(1, Lista, Soma).

% Fila vertical
valor_soma(Lista, H_V, Soma) :-
     H_V == v,
     nth0(0, Lista, Soma).

primeiro_espaco(Conjunto) :-
     var(Conjunto),
     Conjunto = [].

primeiro_espaco(Conjunto) :-
     is_list(Conjunto).

% Construtor da estrutura espaco.
faz_espaco(Soma, Lista, espaco(Soma, Lista)).

% Seletores da estrutura espaco.
soma_de(espaco(Soma, _), Soma).
conteudo_espaco(espaco(_, Lista), Lista).
