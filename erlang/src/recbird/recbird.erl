%% recbird: A Dynamic Record Inferring Erlang Parse Transform
%%

%% Copyright (c) 2006 Caoyuan Deng
%% 
%% Permission is hereby granted, free of charge, to any person obtaining
%% a copy of this software and associated documentation files (the
%% "Software"), to deal in the Software without restriction, including
%% without limitation the rights to use, copy, modify, merge, publish,
%% distribute, sublicense, and/or sell copies of the Software, and to 
%% permit persons to whom the Software is furnished to do so, subject to 
%% the following conditions:
%%
%% The above copyright notice and this permission notice shall be included 
%% in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
%% OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF 
%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. 
%% IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY 
%% CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, 
%% TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE 
%% SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

-module(recbird).
-author("Caoyuan Deng (dcaoyuan@lightpole.net)").
-vsn('0.10').

-export([parse_transform/2]).


-export([make_get_record_field_func_helper/1,
         make_set_record_field_func_helper/1,
         make_rec_field_getter_helper/3,
         make_rec_field_setter_helper/3,
         print_ast_of_form/1,
         print_ast_of_exprs/1
        ]).

-record(context, {module,
                  new_attrs = [],
                  new_funcs = []
}).


parse_transform(Ast, _Options) ->
    {RootAst, Context} = parse_ast(Ast, #context{}),
    #context{module=Module, 
             new_attrs=NewAttrs,
             new_funcs=NewFuncs}=Context,
    {attribute, _, module, ModuleName} = Module,
    %% We should add exports before function node begin
    GetRecordFieldFunc = make_get_record_field_func(ModuleName),
    SetRecordFieldFunc = make_set_record_field_func(ModuleName),
    RootAst1 = lists:reverse(RootAst),
    RootAst2 = [SetRecordFieldFunc|[GetRecordFieldFunc|RootAst1]],
    RootAst3 = NewFuncs ++ RootAst2,
    RootAst4 = NewAttrs ++ lists:reverse(RootAst3),
    %io:format("Ast: ~p~n", [RootAst4]),
    [Module|RootAst4].

%% ===== Top forms ===== %%
parse_ast({attribute, _, module, _ModuleName}=Module, Context) ->
    %% We just keep Module in Context, and will add it back to Ast finally
    Context1 = Context#context{module = Module},
    {undefined, Context1};
parse_ast({attribute, _, record, {Name,Fields}}=Record, Context) ->
    Context1 = add_rec_field_funcs(Name, Fields, Context),
    {Record, Context1};
parse_ast({function, L, Name, Arity, Clauses}, Context) ->
    {C1, Context1} = parse_ast(Clauses, Context),
    {{function, L, Name, Arity, C1}, Context1};

%% ===== record fields ===== %%
%% only <Var> <dot> <atom> match following pattern
parse_ast({record_field, _L, {var, _L1, _VarName}, _Field}=RecField, Context) ->
    Getter = getter_of_field_chain(RecField),
    {Getter, Context};
%% only <atom> <dot> <atom> match following pattern
parse_ast({record_field, _L, {record_field, _, _, _}, _Field}=RecField, Context) ->
    Getter = getter_of_field_chain(RecField),
    {Getter, Context};
%% only <Var> <dot> <atom> "=" Expr match following pattern
parse_ast({match, _L, {record_field, _L1, {var, _L2, _VarName}, _Field}=RecField, Val}, Context) ->
    Setter = setter_of_field_chain(RecField, Val),
    {Setter, Context};
%% only <atom> <dot> <atom> "=" Expr match following pattern
parse_ast({match, _L, {record_field, _L1, {record_field, _, _, _}, _Field}=RecField, Val}, Context) ->
    Setter = setter_of_field_chain(RecField, Val),
    {Setter, Context};


%% ===== normal exprs ===== %%
parse_ast({match, L, Expr1, Expr2}, Context) ->
    {E1, Context1} = parse_ast(Expr1, Context),
    {E2, Context2} = parse_ast(Expr2, Context1),
    {{match, L, E1, E2}, Context2};
parse_ast({cons, L, Expr1, Expr2}, Context) ->
    {E1, Context1} = parse_ast(Expr1, Context),
    {E2, Context2} = parse_ast(Expr2, Context1),
    {{cons, L, E1, E2}, Context2};
parse_ast({op, L, Op, Expr}, Context) ->
    {E1, Context1} = parse_ast(Expr, Context),
    {{op, L, Op, E1}, Context1};
parse_ast({op, L, Op, Expr1, Expr2}, Context) ->
    {E1, Context1} = parse_ast(Expr1, Context),
    {E2, Context2} = parse_ast(Expr2, Context1),
    {{op, L, Op, E1, E2}, Context2};
parse_ast({record, L, Name, Fields}, Context) ->
    {F1, Context1} = parse_ast(Fields, Context),
    {{record, L, Name, F1},Context1};
parse_ast({record, L, OtherVar, Name, Fields}, Context) ->
    {F1, Context1} = parse_ast(Fields, Context),
    {{record, L, OtherVar, Name, F1},Context1};
parse_ast({'catch', L, Expr}, Context) ->
    {E1, Context1} = parse_ast(Expr, Context),
    {{'catch', L, E1}, Context1};
parse_ast({call, L, {remote, L1, ModuleExpr, NameExpr}, ParamExprs}, Context) ->
    {M1, Context1} = parse_ast(ModuleExpr, Context),
    {N1, Context2} = parse_ast(NameExpr, Context1),
    {P1, Context3} = parse_ast(ParamExprs, Context2),
    {{call, L, {remote, L1, M1, N1}, P1}, Context3};
parse_ast({call,L, NameExpr, ParamExprs}, Context) ->
    {N1, Context1} = parse_ast(NameExpr, Context),
    {P1, Context2} = parse_ast(ParamExprs, Context1),
    {{call, L, N1, P1}, Context2};
parse_ast({lc, L, Result, Body}, Context) ->
    {B1, Context1} = parse_ast(Body, Context),
    {R1, Context2} = parse_ast(Result, Context1),
    {{lc, L, R1, B1}, Context2};
parse_ast({block, L, Exprs}, Context) ->
    {E1, Context1} = parse_ast(Exprs, Context),
    {{block, L, E1}, Context1};
parse_ast({'if', L, Clauses}, Context) ->
    {C1, Context1} = parse_ast(Clauses, Context),
    {{'if', L, C1}, Context1};
parse_ast({'case', L, Clauses}, Context) ->
    {C1, Context1} = parse_ast(Clauses, Context),
    {{'case', L, C1}, Context1};
parse_ast({'try', L, Exprs1, Clauses1, Exprs2, AfterExpr}, Context) ->
    {E1, Context1} = parse_ast(Exprs1, Context),
    {C1, Context2} = parse_ast(Clauses1, Context1),
    {E2, Context3} = parse_ast(Exprs2, Context2),
    {A3, Context4} = parse_ast(AfterExpr, Context3),
    {{'try', L, E1, C1, E2, A3}, Context4};
parse_ast({'receive', L, Exprs}, Context) ->
    {E1, Context2} = parse_ast(Exprs, Context),
    {{'receive', L, E1}, Context2};
parse_ast({'receive', L, Exprs, AfterExpr, Body}, Context) ->
    {E1, Context1} = parse_ast(Exprs, Context),
    {A1, Context2} = parse_ast(AfterExpr, Context1),
    {B1, Context3} = parse_ast(Body, Context2),
    {{'receive', L, E1, A1, B1}, Context3};
parse_ast({'fun', L, {clauses, Clauses}}, Context) ->
    {C1, Context1} = parse_ast(Clauses, Context),
    {{'fun', L, {clauses, C1}}, Context1};
parse_ast({'query', L, {lc, L1, Result, Body}}, Context) ->
    {R1, Context1} = parse_ast(Result, Context),
    {B1, Context2} = parse_ast(Body, Context1),
    {{'query', L, {lc, L1, R1, B1}}, Context2};
parse_ast({clause, L, Params, Guards, Exprs}, Context) ->
    {P1, Context1} = parse_ast(Params, Context),
    {E1, Context2} = parse_ast(Exprs, Context1),
    {{clause, L, P1, Guards, E1}, Context2};
parse_ast({generate, L, Pattern, Exprs} , Context)->
    {P1, Context1} = parse_ast(Pattern, Context),
    {E1, Context2} = parse_ast(Exprs, Context1),
    {{generate, L, P1, E1}, Context2};

%% ===== some common patterns ===== %%
parse_ast([], Context) -> 
    {[], Context};
parse_ast({Type, Line, Elems}, Context) ->
    {Elems1, Context1} = parse_ast(Elems, Context),
    Node1 = {Type, Line, Elems1},
    {Node1, Context1};
parse_ast(Nodes, Context) when is_list(Nodes) ->
    {Nodes1, Context1} = lists:foldl(
        fun (X, {NodesX, ContextX}) ->
                case parse_ast(X, ContextX) of
                    {undefined, ContextX1} -> {NodesX, ContextX1};
                    {X1,        ContextX1} -> {[X1|NodesX], ContextX1}
                end
        end, {[], Context}, Nodes),
    {lists:reverse(Nodes1), Context1};
parse_ast(Node, Context) -> {Node, Context}.
    

%% @doc gethter anonymous record field chain (Anomynous RecField tuple has 4 elements. 
%% The result and fields order of Var.a.b.c or Var.a.b.c = 100 will be something like: 
%%     {Var, [a, b, c]}
gather_all_fields_and_topvar({record_field, _, {var, _, _VarName}=Var, Field}, GatheredFields) ->
    {Var, [Field|GatheredFields]};
gather_all_fields_and_topvar({record_field, _, OuterField, Field}, GatheredFields) ->
    gather_all_fields_and_topvar(OuterField, [Field|GatheredFields]).

%% the field stack for RecVar.a.b.c.d should be [a, b, c, d]
getter_of_field_chain({record_field, L, Expr, Field}) ->
    {TopVar, AllFields} = gather_all_fields_and_topvar({record_field, L, Expr, Field}, []),
    getter_of_field_chain_1(TopVar, AllFields, L).

getter_of_field_chain_1(Var, [], _L) ->
    Var;
getter_of_field_chain_1(Var, [Field|T], L) ->
    Var1 = {call, L, {atom, L, get_record_field_recbird}, [Var, Field]},
    getter_of_field_chain_1(Var1, T, L).

setter_of_field_chain({record_field, L, Expr, Field}, Val) ->
    {TopVar, AllFields} = gather_all_fields_and_topvar({record_field, L, Expr, Field}, []),
    AllFields1 = lists:reverse(AllFields),
    setter_of_field_chain_1(TopVar, AllFields1, Val, L).
    
setter_of_field_chain_1(_Var, [], Val, _L) ->
    Val;
setter_of_field_chain_1(Var, [Field|T], Val, L) ->
    Var1 = getter_of_field_chain_1(Var, lists:reverse(T), L),
    Val1 = {call, L, {atom, L, set_record_field_recbird}, [Var1, Field, Val]},
    setter_of_field_chain_1(Var, T, Val1, L). 
    
    
make_get_record_field_func(Module) ->
    {function,
     2,
     get_record_field_recbird,
     2,
     [{clause,
       2,
       [{var,2,'X'},{var,2,'Field'}],
       [],
       [{match,
         3,
         {var,3,'RecordName'},
         {call,
          3,
          {atom,3,atom_to_list},
          [{call,3,{atom,3,element},[{integer,3,1},{var,3,'X'}]}]}},
        {match,
         4,
         {var,4,'FieldName'},
         {call,4,{atom,4,atom_to_list},[{var,4,'Field'}]}},
        {match,
         5,
         {var,5,'GettterFun'},
         {call,
          5,
          {atom,5,list_to_atom},
          [{op,
            5,
            '++',
            {string,5,"get_"},
            {op,
             5,
             '++',
             {var,5,'RecordName'},
             {op,
              5,
              '++',
              {string,5,"_"},
              {op,5,'++',{var,5,'FieldName'},{string,5,"_recbird"}}}}}]}},
        {call,
         6,
         {remote,6,{atom,6,Module},{var,6,'GettterFun'}},
         [{var,6,'X'}]}]}]}.

make_set_record_field_func(Module) ->
    {function,
     2,
     set_record_field_recbird,
     3,
     [{clause,
       2,
       [{var,2,'X'},{var,2,'Field'},{var,2,'Value'}],
       [],
       [{match,
         3,
         {var,3,'RecordName'},
         {call,
          3,
          {atom,3,atom_to_list},
          [{call,3,{atom,3,element},[{integer,3,1},{var,3,'X'}]}]}},
        {match,
         4,
         {var,4,'FieldName'},
         {call,4,{atom,4,atom_to_list},[{var,4,'Field'}]}},
        {match,
         5,
         {var,5,'SettterFun'},
         {call,
          5,
          {atom,5,list_to_atom},
          [{op,
            5,
            '++',
            {string,5,"set_"},
            {op,
             5,
             '++',
             {var,5,'RecordName'},
             {op,
              5,
              '++',
              {string,5,"_"},
              {op,5,'++',{var,5,'FieldName'},{string,5,"_recbird"}}}}}]}},
        {call,
         5,
         {remote,5,{atom,5,Module},{var,5,'SettterFun'}},
         [{var,5,'X'},{var,5,'Value'}]}]}]}.

add_rec_field_funcs(RecName, Fields, #context{new_attrs=NewAttrs,
                                              new_funcs=NewFuncs}=Context) ->
    {_Count, NewAttrs1, NewFuncs1} = lists:foldl(
        fun (Field, {Index, NewAttrsX, NewFuncsX}) -> 
                FieldName = get_field_name(Field),
                {GetterExport, Getter} = make_rec_field_getter(RecName, FieldName, Index),
                {SetterExport, Setter} = make_rec_field_setter(RecName, FieldName, Index),
                {Index + 1, [SetterExport|[GetterExport|NewAttrsX]], [Setter|[Getter|NewFuncsX]]}
        end, {1, NewAttrs, NewFuncs}, Fields),
    Context#context{new_attrs = NewAttrs1, new_funcs = NewFuncs1}.

make_rec_field_getter(RecName, FieldName, Index) ->
    GetterNameStr = "get_" ++ atom_to_list(RecName) ++ "_" ++ atom_to_list(FieldName) ++ "_recbird",
    GetterName = list_to_atom(GetterNameStr),
    Export = {attribute,1,export,[{GetterName,1}]},
    Getter = {function,1,
              GetterName,
              1,
              [{clause,1,
                       [{var,1,'X'}],
                       [],
                       [{call,1,
                              {atom,1,element},
                              [{integer,1,Index + 1},{var,1,'X'}]}]}]},
    {Export, Getter}.

make_rec_field_setter(RecName, FieldName, Index) ->
    SetterNameStr = "set_" ++ atom_to_list(RecName) ++ "_" ++ atom_to_list(FieldName) ++ "_recbird",
    SetterName = list_to_atom(SetterNameStr),
    Export = {attribute,1,export,[{SetterName,2}]},
    Setter = {function,1,
              SetterName,
              2,
              [{clause,1,
                       [{var,1,'X'},{var,1,'Value'}],
                       [],
                       [{call,1,
                              {atom,1,setelement},
                              [{integer,1,Index + 1},{var,1,'X'},{var,1,'Value'}]}]}]},
    {Export, Setter}.

make_get_record_field_func_helper(Module) ->
    FuncStr = "
        get_record_field_recbird(X, Field) ->
            RecordName = atom_to_list(element(1, X)),
            FieldName  = atom_to_list(Field),
            GettterFun = list_to_atom(\"get_\" ++ RecordName ++ \"_\" ++ FieldName ++ \"_recbird\")," ++
            atom_to_list(Module) ++ ":GettterFun(X).",
    print_ast_of_form(FuncStr). 

make_set_record_field_func_helper(Module) ->
    FuncStr = "
        set_record_field_recbird(X, Field, Value) ->
            RecordName = atom_to_list(element(1, X)),
            FieldName  = atom_to_list(Field),
            SettterFun = list_to_atom(\"set_\" ++ RecordName ++ \"_\" ++ FieldName ++ \"_recbird\")," ++
            atom_to_list(Module) ++ ":SettterFun(X, Value).",
    print_ast_of_form(FuncStr). 

make_rec_field_getter_helper(RecName, FieldName, Index) ->
    GetterName = "get_" ++ atom_to_list(RecName) ++ "_" ++ atom_to_list(FieldName) ++ "_recbird",
    FuncStr = GetterName ++ "(X) -> element(" ++ integer_to_list(Index + 1) ++ ", X).", 
    print_ast_of_form(FuncStr). 

make_rec_field_setter_helper(RecName, FieldName, Index) ->
    SetterName = "set_" ++ atom_to_list(RecName) ++ "_" ++ atom_to_list(FieldName) ++ "_recbird",
    FuncStr = SetterName ++ "(X, Value) -> setelement(" ++ integer_to_list(Index + 1) ++ ", X, Value).", 
    print_ast_of_form(FuncStr). 


get_field_name({record_field, _, {atom, _, FieldName}}) ->
    FieldName;
get_field_name({record_field, _, {atom, _, FieldName}, _Val}) ->
    FieldName;
get_field_name({record_field, _, {var, _, _Var}, _RecName, {atom, _, FieldName}}) ->
    FieldName;
get_field_name({record_field, _, {record_field, _, _, _, _}, _RecName, {atom, _, FieldName}}) ->
    FieldName.




%%% Some erlang ast helper functions %%%

print_ast_of_form(FormStr) ->
    {ok, FormTokens, _} = erl_scan:string(FormStr),
    {ok, FormAst} = erl_parse:parse_form(FormTokens),
    io:format("FormAst: ~nAst:~p~n", [FormAst]).
    
print_ast_of_exprs(ExprsStr) ->
    {ok, ExprsTokens, _} = erl_scan:string(ExprsStr),
    {ok, [ExprsAst]} = erl_parse:parse_exprs(ExprsTokens),
    io:format("ExprsAst: ~nAst:~p~n", [ExprsAst]).



