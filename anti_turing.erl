% Программа моделирования на Erlang решения квадратного уравнения в потоке данных

-module(anti_turing).
-export([start_a/0, start_b/0, start_c/0, get_x1/0, get_x2/0]).
-vsn(1.0).

% Построение дерева решения и распределение машины исполнения
% Запуск асинхронных процессов считывания начальных данных
start_a() -> 
    start_d(), 	start_x1(),   start_x2(),
	loop_a(1). 

start_b() ->
	loop_b(1).

start_c() -> 
	loop_c(1).

% Запуск узлов вычислителей
start_d() -> 
    register(d, spawn(fun() -> loop_d(undefined, undefined, undefined) end )),
	start_sqrt().
	
start_sqrt() -> 
    register(sqrt, spawn(fun() -> loop_sqrt() end )).

% Запуск узлав результатов	
start_x1() -> 
    register(x1_memo, spawn(fun() -> loop_x1(undefined, undefined, undefined, undefined) end )).

start_x2() -> 
    register(x2_memo, spawn(fun() -> loop_x2(undefined, undefined, undefined, undefined) end )).


% Содержательная часть движка
loop_a(N) ->    % Передается счетчик выборок
    io:format("Выборка ~w. ",[N]),
	case io:fread("Введите множитель A > ", "~f") of
        {ok, [Num]} when Num == 0.0 -> io:format("Завершаем цепочку расчетов~n",[]), 
                                d ! sqrt ! x1_memo ! x2_memo ! stop;
        {ok, [Num]} when (Num < 0.0) or (Num > 0.0) ->  d ! {a, Num},
                                                        x1_memo ! {a, Num},
                                                        x2_memo ! {a, Num}, 
                    	                                loop_a(N+1);
        {error, FreadError} ->  io:format("Ошибка ввода ~w~n", [FreadError]), loop_a(N)
    end.

loop_b(N) ->    % Передается счетчик выборок
    io:format("Выборка ~w. ",[N]),
	case io:fread("Введите множитель B > ", "~f") of
        {ok,[Num]} ->   {d, a@localhost} ! {b, Num},
                        {x1_memo, a@localhost} ! {b, Num},
                        {x2_memo, a@localhost} ! {b, Num},
                        loop_b(N+1);
        {error, FreadError} ->  io:format("Ошибка ввода~w~n", [FreadError]), 
                                loop_b(N);
        stop -> true
    end.


loop_c(N) ->    % Передается счетчик выборок
    io:format("Выборка ~w. ",[N]),
	case io:fread("Введите множитель C > ", "~f") of
        {ok, [Num]} ->  {d, a@localhost} ! {c, Num},
                        loop_c(N+1);
        {error, FreadError} ->  io:format("Ошибка ввода ~w~n", [FreadError]), 
                                loop_c(N);   
        stop -> true
    end.

loop_d(A,B,C) ->
	receive	
        {a, Acur} when (B == undefined) or (C == undefined) -> loop_d(Acur,B,C);
		{b, Bcur} when (A == undefined) or (C == undefined) -> loop_d(A,Bcur,C);
        {c, Ccur} when (A == undefined) or (B == undefined) -> loop_d(A,B,Ccur);
		{a, Acur} when (B /= undefined) and (C /= undefined) -> 
                                                        Dt = B*B - 4*Acur*C,
                                                        sqrt ! {d, Dt};
        {b, Bcur} when (A /= undefined) and (C /= undefined) -> 
                                                        Dt = Bcur*Bcur - 4*A*C,
                                                        sqrt ! {d, Dt};
		{c, Ccur} when (A /= undefined) and (B /= undefined) ->
                                                        Dt = B*B - 4*A*Ccur, 
                                                        sqrt ! {d, Dt};
        stop -> true	
	end,
    % Возврат начального состояния
	loop_d(undefined, undefined, undefined).		

loop_sqrt() ->
    receive
        {d,D} when D >=0 -> Sqrt = math:sqrt(D),
                        x1_memo ! x2_memo ! {sqrt, Sqrt},
                        loop_sqrt();
        {d,D} when D < 0 -> io:format("Дискреминант отрицательный. Поменяйте начальные данные.",[]);
        stop -> true
    end.

loop_x1(A, B, Sqrt, X) ->   % X-сохраняемое значение memo
    receive
        {a, Acur} ->  loop_x1(Acur, B, Sqrt, X);
        {b, Bcur} ->  loop_x1(A, Bcur, Sqrt, X);
        {sqrt, Sqrtcur} when (A /= undefined) and (B /= undefined) ->  
                                            Xcur = -(B + Sqrtcur)/2/A ,
                                            x1_memo !  {save, Xcur},
            loop_x1(undefined, undefined, undefined, Xcur);
        {get, Pid} -> Pid ! {reply, X}, loop_x1(A, B, Sqrt, X);
        stop -> true
    end.
 
loop_x2(A, B, Sqrt, X) ->  % X-сохраняемое значение memo
    receive
        {a,Acur} -> loop_x2(Acur, B, Sqrt, X);
        {b,Bcur} -> loop_x2(A, Bcur, Sqrt, X);
        {sqrt, Sqrtcur} when (A /= undefined) and (B /= undefined)-> 
                                            Xcur = (-B + Sqrtcur)/2/A ,
                                            x2_memo !  {save, Xcur },
             loop_x2(undefined, undefined, undefined, Xcur);
        {get, Pid} -> Pid ! {reply, X}, loop_x2(A, B, Sqrt, X);
        stop -> true
    end.

% функции для внешего доступа данным memo
get_x1() ->
    {x1_memo, a@localhost } ! {get, self()},
    receive 
        {reply, Answer} -> Answer
    end.
        
get_x2() ->
    {x2_memo, a@localhost } ! {get, self()},
    receive 
        {reply, Answer} -> Answer
    end.

