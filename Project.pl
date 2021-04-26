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
