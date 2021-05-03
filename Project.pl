% Gonçalo Nunes 199229
% Projeto de LP 2020/2021
% Solucionador de Puzzles Kakuro

:- [codigo_comum, puzzles_publicos].

% -----------------------------------------------------------------------
%                     Estrutura: espaco(Soma, Variaveis)
% placeholder
% -----------------------------------------------------------------------

%Construtor da estrutura espaco.
faz_espaco(Soma, Lista, espaco(Soma, Lista)).

% Seletores da estrutura espaco.
% Afirma qual eh a Soma das celulas do espaco
soma_de(espaco(Soma, _), Soma).

% Afirma qual eh o conteudo do espaco
conteudo_espaco(espaco(_, Lista), Lista).

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
     espaco_fila_aux(Fila, Esp, H_V, 0, [], []).

% Se ainda nao houve celulas livres
espaco_fila_aux([P | R], Esp, H_V, _, Vars, Conjunto) :-
     is_list(P),
     length(Vars, N),   % Ver
     N =:= 0,
     valor_soma(P, H_V, Soma),
     espaco_fila_aux(R, Esp, H_V, Soma, Vars, Conjunto).


% Se for uma celula livre, a "direita" de uma restricao
espaco_fila_aux([P | R], Esp, H_V, Soma, Vars, Conjunto) :-
     var(P),
     append(Vars, [P], Novo_Vars),
     espaco_fila_aux(R, Esp, H_V, Soma, Novo_Vars, Conjunto).


% Fim do primeiro espaco dentro da fila
espaco_fila_aux([P | R], Esp, H_V, Soma, Vars, Conjunto) :-
     is_list(P),
     length(Vars, N),
     N =\= 0,
     faz_espaco(Soma, Vars, Esp_Atual),
     primeiro_espaco(Conjunto), % Se for variavel, unifica com lista vazia
     append([Esp_Atual], Conjunto, Atualizado),
     espaco_fila_aux(R, Esp, H_V, Soma, [], Atualizado).

% Caso terminal de espaco_fila_aux. Corresponde a variavel
% Fila ser a lista vazia
espaco_fila_aux([], Esp, _, Soma, Vars, Conjunto) :-
%     length(Vars, N),  % Verifica se tem variaveis
%     N =\= 0,          % para criar o espaco
%     faz_espaco(Soma, Vars, Esp_Atual),
%     append(Conjunto, [Esp_Atual], Resultado),
     ultimo_espaco(Vars, Soma, Esp_Atual),
     append(Conjunto, Esp_Atual, Resultado),
     member(Esp, Resultado).

% Se Vars estiver vazio
%espaco_fila_aux([], _, _, _, _, _) :-
%     fail.

% Fila horizontal
valor_soma(Lista, H_V, Soma) :-
    H_V == h,
    nth0(1, Lista, Soma).

% Fila vertical
valor_soma(Lista, H_V, Soma) :-
     H_V == v,
     nth0(0, Lista, Soma).

% Unifica Conjunto com a lista vazia, se Conjunto for
% uma variavel
primeiro_espaco(Conjunto) :-
     var(Conjunto),
     Conjunto = [].

% Afirma que Conjunto eh uma lista
primeiro_espaco(Conjunto) :-
     is_list(Conjunto).

% Verifica ha um novo espaco para criar
ultimo_espaco(Vars, Soma, [Esp_Atual]) :-
     length(Vars, N),
     N > 0,
     faz_espaco(Soma, Vars, Esp_Atual).
% Unifica o Espaco Atual com a lista vazia
ultimo_espaco(Vars, _, []) :-
     length(Vars, N),
     N =:= 0.

% -----------------------------------------------------------------------
%                    espacos_fila(H_V, Fila, Espacos)
% espacos_fila(H_V, Fila, Espacos), em que Fila eh uma fila (linha ou
% coluna) de uma grelha e e H_V eh um dos atomos h ou v, significa que
% Espacos eh a lista de todos os espaços de Fila, da esquerda para a
% direita.
% -----------------------------------------------------------------------
espacos_fila(H_V, Fila, Espacos) :-
     bagof(X, espaco_fila(Fila, X, H_V), Espacos), !.

espacos_fila(_, _, Espacos) :-
     Espacos = [].
