% Goncalo Nunes 199229
% Projeto de LP 2020/2021
% Solucionador de Puzzles Kakuro
%
%:- [codigo_comum, puzzles_publicos].
%
:- [codigo_comum].


% -----------------------------------------------------------------------
%                     Estrutura: espaco(Soma, Variaveis)
% placeholder
% -----------------------------------------------------------------------

% Construtor da estrutura espaco.
faz_espaco(Soma, Lista, espaco(Soma, Lista)) :-
     length(Lista, N),
     N > 0.

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
% lista ordenada cujos elementos sao as permutacoes das combinacoes N a
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
% espaco de Fila.
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
     append(Conjunto, [Esp_Atual], Atualizado),
     valor_soma(P, H_V, Nova_Soma),
%     append([Esp_Atual], Conjunto, Atualizado),
     espaco_fila_aux(R, Esp, H_V, Nova_Soma, [], Atualizado).

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
% coluna) de um puzzle e H_V eh um dos atomos h ou v, significa que
% Espacos eh a lista de todos os espaços de Fila, da esquerda para a
% direita.
% -----------------------------------------------------------------------
espacos_fila(H_V, Fila, Espacos) :-
     bagof(X, espaco_fila(Fila, X, H_V), Espacos).
%     bagof(X, espaco_fila(Fila, X, H_V), Espacos), !.

espacos_fila(_, _, Espacos) :-
     Espacos = [].

%-----------------------------------------------------------------------
%                     espacos_puzzle(Puzzle, Espacos)
% espacos_puzzle(Puzzle, Espacos), em que Puzzle eh um puzzle, significa
% que Espacos eh a lista de espaços de Puzzle
% -----------------------------------------------------------------------
espacos_puzzle(Puzzle, Espacos) :-
     bagof(X,
             Fila^(member(Fila, Puzzle), espacos_fila(h, Fila, X)),
             Horizontais),

     mat_transposta(Puzzle, Transposta),
     bagof(X,
             Fila2^(member(Fila2, Transposta), espacos_fila(v, Fila2, X)),
             Verticais),

     append(Horizontais, Verticais, Resultado),
     exclude(lista_vazia, Resultado, Res2),
     flatten(Res2, Espacos).

% Afirma se o termo que recebe eh uma lista vazia
lista_vazia([]).

% -----------------------------------------------------------------------
%               numeros_comuns(Lst_Perms, Numeros_comuns)
% numeros_comuns(Lst_Perms, Numeros_comuns), em que Lst_Perms eh uma
% lista de permutacoes, significa que Numeros_comuns eh uma lista de
% pares (pos, numero), significando que todas as listas de Lst_Perms
% contem o numero numero na posicao pos.
% -----------------------------------------------------------------------
numeros_comuns([P | R], Numeros_comuns) :-
     numeros_comuns_aux(P, 1, R, [], Numeros_comuns).

% Caso terminal da recursao, a chegada ao fim da lista de permutacoes.
% O acumulador e os Numeros_comuns sao unificados
numeros_comuns_aux([], _, _, Numeros_comuns, Numeros_comuns).

% Verifica, recursivamente, se os membros de uma lista estao contidos
% em todas as outras listas, na lista de listas Resto, e se tem o
% mesmo indice em todas
numeros_comuns_aux([P | R], Pos, Resto, Acumulador, Numeros_comuns) :-
%     primeiro_espaco(Numeros_comuns),
     forall(member(Y, Resto), nth1(Pos, Y, P)),
     Par = (Pos, P),
     append(Acumulador, [Par], Nov_Ac),
%     append(Numeros_comuns, [Par], Novo_comuns),
     Prox_Pos is Pos + 1,
     numeros_comuns_aux(R, Prox_Pos, Resto, Nov_Ac, Numeros_comuns).

%numeros_comuns_aux([P | R], Pos, Resto, [[Par] | Numeros_comuns]) :-
%     primeiro_espaco(Numeros_comuns),
%     forall(member(Y, Resto), nth1(Pos, Y, P)),
%     Par = (Pos, P),
%     append(Numeros_comuns, [Par], Novo_comuns),
%     Prox_Pos is Pos + 1,
%     numeros_comuns_aux(R, Prox_Pos, Resto, Numeros_comuns).


% Se o elemento nao estava em todas as sublistas de Resto e na mesma
% posicao em todas
numeros_comuns_aux([_ | R], Pos, Resto, Acumulador, Numeros_comuns) :-
     Prox_Pos is Pos + 1,
     numeros_comuns_aux(R, Prox_Pos, Resto, Acumulador, Numeros_comuns).

%[P | R],
%El = blah,
%Pos = blah,
%forall(member(Y, R), nth0(Pos, Y, El)).
%findall(X, forall(member(Y, R), nth0(Pos, Y, El))
%findall(X, (member(Y, R), nth0(X, Y, Var), Var =:= El

% -----------------------------------------------------------------------
%        espacos_com_posicoes_comuns(Espacos, Esp, Esps_com)
% espacos_com_posicoes_comuns(Espacos, Esp, Esps_com), em que Espacos
% eh uma lista de espacos e Esp eh um espaco, significa que Esps_com eh
% a lista de esp aços com variaveis em comum com Esp, exceptuando Esp.
% -----------------------------------------------------------------------
espacos_com_posicoes_comuns(Espacos, Esp, Esps_com) :-
     conteudo_espaco(Esp, Conteudo),

     bagof(X, Z^(member(X, Espacos), conteudo_espaco(X, Z),
               pos_comuns_aux(Conteudo, Z)), Temp),

     exclude(=(Esp), Temp, Temp2), % Retira o Espaco de "partida"
     flatten(Temp2, Esps_com),!.

% Afirma que nao encontrou espacos com variaveis em comum
espacos_com_posicoes_comuns(_, _, Esps_com) :-
     Esps_com = [].

% Afirma que nao ha posicoes comuns
pos_comuns_aux([], _) :- fail.

% Afirma que ha pelo menos uma posicao comum
pos_comuns_aux([P | _], Cont2) :-
     membro(P, Cont2),!.

pos_comuns_aux([P|R], Cont2) :-
     \+ membro(P, Cont2),
     pos_comuns_aux(R, Cont2).


% Afirma que que nao eh membro da lista
membro(_, []) :- fail.

% Afirma que eh membro da lista, sem unificar
membro(El, [P | _]) :-
     El == P.

membro(El, [P | R]) :-
     El \== P,
     membro(El, R).

% -----------------------------------------------------------------------
%             permutacoes_soma_espacos(Espacos, Perms_soma)
% permutacoes_soma_espacos(Espacos, Perms_soma), em que Espacos eh uma
% lista de espacos, significa que Perms_soma eh a lista de listas de 2
% elementos, em que o primeiro elemento eh um espaco de Espacos e o
% segundo eh a lista ordenada de permutacoes cuja soma eh igual a soma
% do espaco.
% -----------------------------------------------------------------------
permutacoes_soma_espacos(Espacos, Perms_soma) :-
     permutacoes_soma_espacos_aux(Espacos, [], Perms_soma).

% Caso terminal. Unifica o acumulador com Perms_soma
permutacoes_soma_espacos_aux([], Perms_soma, Perms_soma).

% Faz a lista desejada com o primeiro espaco que encontra
% e as permutacoes possiveis e repete para o resto dos espacos
permutacoes_soma_espacos_aux([Esp | R], Acc, Perms_soma) :-
%     nth0(0, P, Esp), % Tem se de retirar o espaco da lista
     conteudo_espaco(Esp, Conteudo),
     soma_de(Esp, Soma),
     length(Conteudo, N), % Num de elementos a combinar/permutar

     permutacoes_soma(N, [1,2,3,4,5,6,7,8,9], Soma, Perms),
     append([Esp], [Perms], Res),
     append(Acc, [Res], Acc1),

     permutacoes_soma_espacos_aux(R, Acc1, Perms_soma).
