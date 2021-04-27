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
