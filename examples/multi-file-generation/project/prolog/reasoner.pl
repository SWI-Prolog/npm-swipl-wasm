/* Copyright [2021] Initial copyright holders by country: 
LodgeIT (AU), AORA Law (UK), Bob Kowalski (UK), Miguel Calejo (PT), Jacinto Dávila (VE)

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

:- module(reasoner,[query/4, query_with_facts/5, query_once_with_facts/5, explanation_node_type/2, render_questions/2,
    run_examples/0, run_examples/1, myClause2/9, myClause/4, taxlogWrapper/10, niceModule/2, refToOrigin/2,
    isafter/2, is_not_before/2, isbefore/2, immediately_before/2, same_date/2, subtract_days/3, this_year/1, uk_tax_year/4, in/2,
    isExpressionFunctor/1, set_time_of_day/3, start_of_day/2, end_of_day/2, is_days_after/3, is_1_day_after/2, unparse_time/2
    ]).

/** <module> Tax-KB reasoner and utils
@author Miguel Calejo
*/

:- use_module(library(aggregate)).

:- use_module(kp_loader).
:- use_module(le_answer).

:- thread_local do_not_fail_undefined_preds/0. 

% query(AtGoal,Unknowns,ExplanationTerm,Result)
%  Result will be true/false/unknown
query(Goal,Questions,E,Result) :- 
    query_with_facts(Goal,[],false,Questions,E,Result).

%! query_with_facts(+Goal,?FactsListOrExampleName,-Unknowns,?ExplanationTemplate,-Result)
query_with_facts(Goal,Facts,Questions,E,Outcome) :-
    query_with_facts(Goal,Facts,false,Questions,E,Outcome).

%! query_with_facts(+Goal,?FactsListOrExampleName,+OnceUndo,-Unknowns,-ExplanationTemplate,-Result)
%  query considering the given facts (or accumulated facts of all scenarios the given example name)
%  if OnceUndo, only one solution, and time execution is limited
%  ExplanationTemplate determines Logical English or taxlog (Prolog syntax) explanation nodes
%  Result will be true/false/unknown
%  This is NOT reentrant
query_with_facts(Goal,Facts_,OnceUndo,unknowns(Unknowns),E,Outcome) :- %trace, 
    must_be(boolean,OnceUndo),
    (Goal=at(G,M__) -> atom_string(M_,M__) ; 
        myDeclaredModule(M_) -> Goal=G; 
        (print_message(error,"No knowledge module specified"-[]), fail)),
    context_module(Me),
    (shouldMapModule(M_,M)->true;M=M_),
    (is_list(Facts_)-> Facts=Facts_; example_fact_sequence(M,Facts_,Facts)),
    Caller = Me:(
        i(at(G,M),OnceUndo,U,Result_),
        Result_=..[Outcome,E_],
        expand_explanation_refs(E_,Facts,E)
        ),
    retractall(hypothetical_fact(_,_,_,_,_,_)),
    (OnceUndo==true -> (true, once_with_facts(Caller, M, Facts, true)) ; (true, call_with_facts(Caller, M, Facts))),
    list_without_variants(U,Unknowns_), % remove duplicates, keeping the first clause reference for each group
    mapModulesInUnknwons(Unknowns_,Unknowns).

%! query_once_with_facts(+Goal,?FactsListOrExampleName,-Unknowns,-Explanation,-Result)
%  query considering the given facts (or accumulated facts of all scenarios the given example name), undoes them at the end; limited execution time
query_once_with_facts(Goal,Facts_,Questions,E,Outcome) :-
    query_with_facts(Goal,Facts_,true,Questions,E,Outcome).

%! render_questions(+Unknowns,-Questions) is det
%   Transform unknown goals ( at(G,KP)/c(Cref) )into question(...) terms
render_questions(unknowns(U),Q) :- !, nonvar(U), render_questions(U,Q).
render_questions([U1|Un],[Q1|Qn]) :- !, render_question(U1,Q1), render_questions(Un,Qn).
render_questions([],[]).

render_question(at(U,M_)/_,question(U,Q)) :- (shouldMapModule(M_,M)->true;M=M_), catch(M:question(U,QT),_,fail), !, 
    (QT=Format-Args -> format(string(Q),Format,Args); Q=QT).
render_question(at(U,M_)/_,question(U,Q,Answer)) :- (shouldMapModule(M_,M)->true;M=M_), catch(M:question(U,QT,Answer),_,fail), !, 
    (QT=Format-Args -> format(string(Q),Format,Args); Q=QT).
render_question(G/_,Q) :- format(string(Q)," Is ~w true?",[G]).

% list_without_variants(+L,-NL) 
% Remove duplicates from list L, without binding variables, and keeping last occurrences in their original order
list_without_variants([X/Cref|L],[X/Cref|NL]) :- !, remove_all_variants(L,X,LL), list_without_variants(LL,NL).
list_without_variants([],[]).
% remove_all_variants(+List,+Term,-NewList)
remove_all_variants(L,T,NL) :- select(X/_Cref,L,LL), variant(X,T), !, remove_all_variants(LL,T,NL).
remove_all_variants(L,_,L).

niceModule(Goal,NiceGoal) :- nonvar(Goal), Goal=at(G,Ugly), moduleMapping(Nice,Ugly), !, NiceGoal=at(G,Nice).
niceModule(G,G).

mapModulesInUnknwons([G/Cref|U], [NG/Cref|NU]) :- !, niceModule(G,NG), mapModulesInUnknwons(U,NU).
mapModulesInUnknwons([],[]).

% i(+AtGoal,+OnceTimed,-Unknowns,-ExplainedResult) always succeeds, with result true(Explanation) or false(Explanation)
% top level interpreter predicate; true with Unknowns\=[].... means 'unknown'
i( at(G,KP),OnceTimed,U,Result) :- % hack to use the latest module version on the SWISH window
    shouldMapModule(KP,UUID), !,
    i( at(G,UUID),OnceTimed,U,Result).
i( at(G,KP),OnceTimed,Unknowns,Result) :- !,
    reset_errors,
    context_module(M), 
    nextGoalID(ID),
    Limit=0.5, % max seconds
    Caller = i(at(G,KP),M,top_goal,top_clause,U__,E__),
    (OnceTimed==true -> IG = call_with_time_limit( Limit, Caller) ; IG = Caller), 
    ( catch( (IG,E_=E__,U=U__), time_limit_exceeded, (E_=[], U=[time_limit_exceeded], print_message(warning,"Time limit of ~w seconds exceeded by ~w at ~w"-[Limit,G,KP])) ) *-> 
        (expand_failure_trees_and_simp(E_,FailedUnknowns,E), Result=true(E)) ;
        (expand_failure_trees_and_simp([f(ID,_,_,_)],FailedUnknowns,E), U=[], Result=false(E)) ),
    append(U,FailedUnknowns,U_),
    maplist(niceModule,U_,Unknowns).
i( G,_,_,_) :- print_message(error,"Top goal ~w should be qualified with ' at knowledge_page'"-[G]), fail.

% i(+Goal,+AlreadyLoadedAndMappedModule,+CallerGoalID,+CallerClauseRef,-Unknowns,-Why) 
% failure means false; success with empty Unknowns list means true; 
% Unknowns contains a list of at(GoalOrErrorTerm,Module)/c(CallerClauseRef)  ..c a hack to avoid a SWISH rendering bug
% otherwise, result unknown, depending on solutions to goals in Unknowns; 
%  explanation is a list of proof-like trees: 
%   s(nodeLiteral,Module,ClauseRef,childrenNodes); (s)success) [] denotes.. some self-evident literal; 
%   u(nodeLiteral,Module,CallerClauseRef,[]); u)nknown (or system predicate floundering), basically similar to s
%  if nextGoalID(ID), i(G,...) fails with zero solutions, there will be a failed tree asserted with root failed(ID,...)
%  failures for not(G) goals will also leave asserted a fact failed_success(ID,Unknowns,Why)
%  successes for not(G) goals will have a Why with the underlying failure, f(NegatedGoalID,Module,CallerID,FreeVar); 
%  these can be expanded by expand_failure_trees into f(G,Module,CallerClausRef,Children)
%  there may be orphan failed(ID,...) facts, because we're focusing on solution-less failures only
%  this predicate is NOT thread safe
%i(G,M,_,_,_,_) :- nextGoalID(ID), writeln(ID-G/M), fail.
%i(G,M,_,_,_,_) :- nextGoalID(ID), print_message(informational,"~w"-[ID-G/M]), fail.
i(G,M,_,_,_,_) :- var(G), !, throw(variable_call_at(M)).
i(true, _, _, _, U, E) :- !, U=[], E=[].
i(false, _, _, _, _U, _E) :- !, fail.
i(and(A,B), M, CID, Cref, U, E) :- !, i((A,B),M,CID,Cref,U,E).
i((A,B), M, CID, Cref, U, E) :- !, i(A,M,CID,Cref,U1,E1), i(B,M,CID,Cref,U2,E2), append(U1,U2,U), append(E1,E2,E).
i(or(A,B), M, CID, Cref, U, E) :- !, i((A;B),M,CID,Cref,U,E).
i((A;B), M, CID, Cref, U, E) :- !, (i(A,M,CID,Cref,U,E) ; i(B,M,CID,Cref,U,E)).
i(must(I,M), Mod, CID, Cref, U, E) :- !, i(then(I,M), Mod, CID, Cref, U, E).
i(\+ G,M,CID, Cref, U,E) :- !, i( not(G),M,CID,Cref,U,E).
i(not(G), M, CID, Cref, NotU, NotE) :- !, 
    newGoalID(NotID),
    % our negation as failure requires no unknowns:
    ( i( G, M, NotID, Cref, U, E) -> (
        assert( failed(NotID,M,CID,Cref,not(G))),
        assert( failed_success(NotID,U,E)),
        fail
    ) ; (
        NotE = [f(NotID,M,CID,_NotHere_TheyAreAsserted)], NotU=[]
    )).
i(!,_,_,_,_,_) :- throw(no_cuts_allowed).
i(';'(C->T,Else), M, CID, Cref, U, E) :- !,% should we forbid Prolog if-then-elses..?
    nextGoalID(ID),
    ( i(C,M,CID,Cref,UC,EC) -> (
        i(T,M,CID,Cref,UT,ET), 
        append(UC,UT,U), append(EC,ET,E)
    ) ; (
        % any further failures under the (failed) condition?
        ((nextGoalID(Other), Other\=ID) -> EC=[f(ID,M,CID,_NotYet)] ; EC=[]),
        i(Else,M,CID,Cref,U,EE), append(EC,EE,E) 
    )).
i((If->Then),M,CID,Cref,U,E) :- !, 
    i((If->Then;fail),M,CID,Cref,U,E).
% i(then(if(C),else(T,Else)), M, CID, Cref, U, E) :- !,
%     nextGoalID(ID),
%     (i(C,M,CID,Cref,UC,EC) *-> (
%         i(T,M,CID,Cref,UT,ET), 
%         append(UC,UT,U), append(EC,ET,E)
%     ) ; (
%         ((nextGoalID(Other), Other\=ID) -> EC=[f(ID,M,CID,_NotYet)] ; EC=[]),
%         i(Else,M,CID,Cref,U,EE), % no unknowns under C for sure
%         append(EC,EE,E)
%     )).
% Now simply using the following, because the above loses explanations for successful C and failed Then
i(then(if(C),else(T,Else)), M, CID, Cref, U, E) :- !,
    i((C,T;not(C),Else), M, CID, Cref, U, E).
i(then(if(C),Then),M,CID,Cref,U,E) :- !, i(then(if(C),else(Then,true)),M,CID,Cref,U,E).
% sometimes this is not used... SWI seems to expands forall(X,C) into \+ (X, \+C)
i(forall(A,B),M,CID,Cref,U,E) :- !, 
    E=[s(forall(A,B),M,meta,Children)],
    newGoalID(ForID),
    findall(X, (
        i(A,M,ForID,Cref,UA,EA), 
        nextGoalID(ID),
        (i(B,M,ForID,Cref,UB,EB) -> (
            append(UA,UB,Ui),append(EA,EB,Ei),X=Ui/Ei
        ) ; (
            % failed; was there a relevant failure under B?
            ((nextGoalID(Other), Other\=ID) -> EB=[f(ID,M,CID,_NotYet)] ; EB=[]),
            append(EA,EB,Ei),
            X=failed(UA/Ei)
            ))
        ), Tuples),
    (member(failed(UB/Ei),Tuples) -> (
        assert( failed(ForID,M,CID,Cref,forall(A,B))),
        assert( failed_success(ForID,UB,Ei)), 
        fail
    ) ; (
        findall(Ui,member(Ui/_,Tuples),U_), append(U_,U),
        findall(Ei,member(_/Ei,Tuples),Children_), append(Children_,Children)
    )).
i(setof(X,G,L),M,CID,Cref,U,E) :- !, E=[s(setof(X,G,L),M,meta,Children)],
    wrapTemplateGoal(G,M,CID,Cref,Ui,Ei,Wrapped), %TODO: should we introduce an explicit failed node for aggregates?
    setof(X/Ui/Ei, Wrapped, Tuples),
    squeezeTuples(Tuples,L,U,Children).
i(bagof(X,G,L),M,CID,Cref,U,E) :- !, E=[s(bagof(X,G,L),M,meta,Children)],
    wrapTemplateGoal(G,M,CID,Cref,Ui,Ei,Wrapped),
    bagof(X/Ui/Ei, Wrapped, Tuples),
    squeezeTuples(Tuples,L,U,Children).
i(aggregate(Template,G,Result),M,CID,Cref,U,E) :- !, E=[s(aggregate(Template,G,Result),M,meta,Children)],
    % uses a bit too much of SWI internals at swipl-devel/library/aggregate.pl
    % note that aggregate/3 fails when there are no solutions, unlike aggregate_all (for count and sum)
    aggregate:template_to_pattern(bag, Template, Pattern, M:G, Goal, Aggregate),
    i(bagof(Pattern, Goal, List),M,CID,Cref,U_,[s(_Bagof,_M,_ClauseRef,Children_)]),
    catch( ( aggregate:aggregate_list(Aggregate, List, Result), U=U_, Children=Children_ ), 
        error(instantiation_error,_Cx), 
        (append(U_,[at(instantiation_error(G),M)/c(Cref)],U), append(Children_,[u(instantiation_error(G),M,unknown,[])],Children)) 
        ).
i(aggregate_all(Template,G,Result),M,CID,Cref,U,AggrE) :- !, AggrE=[s(aggregate_all(Template,G,Result),M,meta,E)],
    % uses a bit too much of SWI internals at swipl-devel/library/aggregate.pl
    aggregate:template_to_pattern(all, Template, Pattern, M:G, Goal, Aggregate),
    i(findall(Pattern, Goal, List),M,CID,Cref,U_,E_),
    catch( ( aggregate:aggregate_list(Aggregate, List, Result), U=U_, E=E_ ), 
        error(instantiation_error,_Cx), 
        (append(U_,[at(instantiation_error(G),M)/c(Cref)],U), append(E_,[u(instantiation_error(G),M,unknown,[])],E)) 
        ).
i(findall(X,G,L),M,CID,Cref,U,E) :- !, 
    newGoalID(FindallID),
    findall(X/Ui/Ei, i(G,M,FindallID,Cref,Ui,Ei), Tuples), 
    (Tuples==[] -> ( % we want to know why there were no solutions:
        L=[], E = [f(FindallID,M,CID,_NotHere_TheyAreAsserted)], U=[]
        ) ; (
        squeezeTuples(Tuples,L,U,Children),
        E=[s(findall(X,G,L),M,meta,Children)]
        )),
    ((E=[],U=[]) -> true ; % findall succeeds always, so we keep its last and only solution's explanation:
        (assert(failed(FindallID,M,CID,Cref,findall(X,G,L))), assert(failed_success(FindallID,U,E)))
    ).
% questions are now annotation facts for rendering unknowns, not goals, so this is commented out:
% i(Q,M,_CID,Cref,U,E) :- functor(Q,question,N), (N=1;N=2), !, 
%     Q=..[_,Q_|_],
%     (Q_=Format-Args -> format(string(Q__),Format,Args); Q_=Q__),
%     U=[at(Q__,M)], E=[u(at(Q__,M),M,Cref,[])].
i(M:G,Mod,CID,Cref,U,E) :- !, i(at(G,M),Mod,CID,Cref,U,E).
i(G,M,CID,Cref,U,E) :- system_predicate(G), !, 
    evalArgExpressions(G,M,NewG,CID,Cref,Uargs,E_),
    % floundering originates unknown:
    catch(( myCall(M:NewG), U=Uargs, E=E_), 
        error(_Error,_Cx), 
        (append(Uargs,[at(instantiation_error(G),M)/c(Cref)],U), append(E_,[u(instantiation_error(G),M,Cref,[])],E) )).
i(at(G,KP),M,CID,Cref,U,E) :- shouldMapModule(KP,UUID), !, 
    i(at(G,UUID),M,CID,Cref,U,E). % use SWISH's latest editor version
i(At,Mod,CID,Cref,U,E) :- At=at(G,M_), !, 
    atom_string(M,M_),
    ( (loaded_kp(M); hypothetical_fact(M,_,_,_,_,_)) -> 
                i(G,M,CID,Cref,U,E) ; 
                (U=[At/c(Cref)], E=[u(At,Mod,Cref,[])] )).
i(G,M,_CID,Cref,U,E) :- unknown(G,M), do_not_fail_undefined_preds, !, 
    (U=[at(G,M)/c(Cref)],E=[ u(at(G,M),M,Cref,[]) ]).
%TODO: on(G,2020) means "G true on some instant in 2020"; who matches that with '20210107' ? check for clauses and hypos
i(G,M,CID,Cref,U,E) :- 
    newGoalID(NewID), create_counter(Counter),
    LastSolutionHolder = hacky(none),
    (true ;( % before failing, save our failure information 
        \+ catch(M:irrelevant_explanation(G),_,fail),
        get_counter(Counter,Nsolutions),
        assert(failed(NewID,M,CID,Cref,G)),
        (Nsolutions==0 -> true ; (
            % we keep the explanation for our last solution, in case no more failure explanations are available for our ancestors
            % and we want some path to the reasons for that failure; this is costly, but less than the full suspects tree
            arg(1,LastSolutionHolder,U_+E_),
            assert(failed_success(NewID,U_,E_))
            )),
        fail
        )),
    evalArgExpressions(G,M,NewG,CID,Cref,Uargs,Eargs), % failures in the expression (which would be weird btw...) stay directly under CID
    myClause(NewG,M,B,Ref,IsProlog,_URL,LocalE),
    (IsProlog==false -> i(B,M,NewID,Ref,U_,Children_) ; (
        catch( myCall(B), error(Error,_), (U_=[at(Error,M)/c(Cref)])), % should this call be qualified with M? What when M is the SWISH module...?
        (var(U_)->U_=[];true),
        Children_=LocalE
    )),
    inc_counter(Counter), % one more solution found; this is a nonbacktrackable operation
    append(Uargs,U_,U),
    (catch(M:irrelevant_explanation(NewG),_,fail) -> E=Eargs ; (E=[s(G,M,Ref,Children)], append(Eargs,Children_,Children) )),
    % we keep the explanation and unknowns for the last solution:
    ((E=[],U=[]) -> true ; copy_term(U+E,U_+E_), nb_setarg(1,LastSolutionHolder,U_+E_)).

% unknown(+Goal,+Module) whether the knowledge source is currently unable to provide a result 
unknown(G,M) :- var(G), !, throw(variable_unknown_call_at(M)).
unknown(on(G,_Time),M) :- !, unknown(G,M).
unknown(G,M) :- functor(G,F,N),functor(GG,F,N), \+ myClause2(GG,_,M,_,_,_,_,_,_).


myCall(G) :- sandbox:safe_call(G).

:- thread_local last_goal_id/1, failed/5, failed_success/3.

nextGoalID(ID) :- 
    (last_goal_id(Old) -> true ; Old=0), ID is Old+1.
newGoalID(ID) :- 
    (retract(last_goal_id(Old)) -> true ; Old=0), ID is Old+1, assert(last_goal_id(ID)).

% destructive counters, may also be used as destructive variables that do not lose their value during repeat/fail loops
create_counter(counter(0)).
get_counter(counter(N),N).
set_counter(Counter,N) :- Counter=counter(_), nb_setarg(1,Counter,N).
inc_counter(Counter,N) :- get_counter(Counter,N), NewN is N+1, nb_setarg(1,Counter,NewN).
inc_counter(Counter) :- inc_counter(Counter,_).

evalArgExpressions(G,M,NewG,CID,Cref,U,E) :- 
    G=..[F|Args], 
    maplist(evalExpression(M,CID,Cref),Args,Results,Us,Es),
    NewG=..[F|Results], 
    append(Us,U), append(Es,E).

% evalExpression(+Module,+CallerID,+CallerClauseRef,+Expression,-Result,+CallerID,+CallerClauseRef,-Unknowns,-WhyExplanation) expands (only) user functions
% TODO: add arithmetic expressions too...?
evalExpression(_M,_CID,_Cref,X,X,[],[]) :- var(X), !.
evalExpression(M,CID,Cref,Exp,R,U,[s(function(Exp),M,Ref,Children)]) :- M:clause(function(Exp,R),Body,Ref), !,
    once( i(Body,M,CID,Cref,U,Children) ).
evalExpression(M,_CID,Cref,Exp,R,U,Why) :- Exp=..[F,_|_], isExpressionFunctor(F), !, 
    catch((R is Exp, U=[], Why=[]), Ex, ( U=[at(instantiation_error(Ex),M)/c(Cref)], Why=[u(instantiation_error(Ex),M,Cref,[])] )).
evalExpression(_M,_CID,_,X,X,[],[]).

isExpressionFunctor(F) :- memberchk(F,[+,-,*,/]).


%wrapTemplateGoal(+Gtemplate,+Module,+CallerID,+CallerClauseRef,+Unknowns,+Explanation,-WrappedGtemplate)
% e.g. X^Y^g --> i(X^Y^i(g,Module,CID,Cref,Unknowns,Explanation))
wrapTemplateGoal(G,M,CID,Cref,U,E,i(G,M,CID,Cref,U,E)) :- var(G), !.
wrapTemplateGoal(V^G,M,CID,Cref,U,E,V^Wrapped) :- !, wrapTemplateGoal(G,M,CID,Cref,U,E,Wrapped).
wrapTemplateGoal(G,M,CID,Cref,U,E,i(G,M,CID,Cref,U,E)).

%squeezeTuples(+Tuples,-ResultsList,-Unknowns,-Explanations)
squeezeTuples(Tuples,L,U,Es) :-
    findall(X, member(X/_/_,Tuples), L), 
    findall(Ui, member(_/Ui/_,Tuples), U_), append(U_,U),
    findall(Ei, member(_/_/Ei,Tuples), Es_), append(Es_,Es).

myClause(H,M,Body,Ref) :- myClause(H,M,Body,Ref,_,_,_).

% myClause(+Head,+Module,-Body,-Ref,-IsProlog,-OriginURL,-LocalExplanation)  IsProlog is true if the body should be called directly, without interpretation
myClause(on(H,Time),M,Body,Ref,IsProlog,URL,E) :- !, myClause2(H,Time,M,Body,Ref,IsProlog,URL,E,_LE_line).
myClause(H,M,Body,Ref,IsProlog,URL,E) :- myClause2(H,_Time,M,Body,Ref,IsProlog,URL,E,_LE_line).

% Supports the injecting of facts for a query:
:- thread_local hypothetical_fact/6. % Module, FactTemplate, Fact, ClauseLikeBody, FakeClauseRef, redefine/extend

% myClause2(PlainHead,Time,Module,Body,Ref,IsProlog,URL,LocalExplanation, LE_line)
myClause2(H,Time,M,Body,Ref,IsProlog,URL,E, Line) :- 
    (nonvar(Ref) -> clause_property(Ref,module(M)) ; true),
    (hypothetical_fact(M,H,_,_,_,extend) -> % allow existing facts and rules to persist even with similar hypos:
        (hypothetical_fact(M,H,H,Body_,Ref,_) ; M:clause(H,Body_,Ref)) ; %... or override them:
        (hypothetical_fact(M,H,Fact,Body_,Ref,_) *-> H=Fact ; M:clause(H,Body_,Ref))
    ),
    % hypos with rules cause their bodies to become part of our resolvent via Body:
    taxlogWrapper(Body_,_ExplicitTime,Time,M,Body,Ref,IsProlog,URL,E, Line).

% taxlogWrapper(RawBody,ExplicitTime,Time,Module,Body,ClauseRef,IsProlog,URL,E,LE_Line)
% keep this in sync with syntax.pl
taxlogWrapper(targetBody(Body,Explicit,Time_,URL,E_,L),Explicit,Time,M,Body,Ref,IsProlog,URL,E, L) :- (Body=call(_);Body==true), !,
    Time=Time_, IsProlog=true, E=[s(E_,M,Ref,[])].
taxlogWrapper(targetBody(Body,Explicit,Time_,URL,E, L),Explicit,Time,_M,Body,_Ref,IsProlog,URL,E, L) :- !, Time=Time_, IsProlog=false.
taxlogWrapper(Body_,false,_Time,_M,Body,_Ref,IsProlog,URL,E, _) :- Body_=Body,IsProlog=true,E=[],URL=''.

refToOrigin(Ref,URL) :-
    blob(Ref,clause), 
    myClause2(_H,_Time,Module_,_Body,Ref,_IsProlog,URL_,_E, _),
    !,
    (moduleMapping(Module,Module_)-> true ; Module=Module_),
    (is_absolute_url(URL_) -> URL=URL_; (
        sub_atom(Module,_,_,0,'/') -> atomic_list_concat([Module,URL_],URL) ; URL=Module % atomic_list_concat([Module,'/',URL_],URL)
        )).
refToOrigin(Ref_,Ref) :- term_string(Ref_,Ref).

% refToSourceAndOrigin(ClauseRef,-SourceCode,-TextOriginURL)
refToSourceAndOrigin(Ref,Source,Origin) :-
    refToOrigin(Ref,Origin),
    ((blob(Ref,clause),clause(H,B,Ref)) -> (
        with_output_to(string(Source),portray_clause((H:-B)))

    )
        ; Source="").

:- multifile prolog:meta_goal/2. % for xref
prolog:meta_goal(at(G,M),[M_:G]) :- (nonvar(M) -> atom_string(M_,M) ; M=M_).
% next two handled by declare_our_metas:
prolog:meta_goal(on(G,_Time),[G]).
prolog:meta_goal(targetBody(G,_,_,_,_,_),[G]).
%prolog:meta_goal(because(G,_Why),[G]).
prolog:meta_goal(and(A,B),[A,B]).
prolog:meta_goal(or(A,B),[A,B]).
prolog:meta_goal(must(A,B),[A,B]).
prolog:meta_goal(not(A),[A]).
prolog:meta_goal(then(if(C),else(T,Else)),[C,T,Else]).
prolog:meta_goal(then(if(C),Then),[C,Then]) :- Then\=else(_,_).
prolog:meta_goal(aggregate(_,G,_),[G]). % is this necessary...?
prolog:meta_goal(aggregate_all(_,G,_),[G]). % is this necessary...?

:- multifile prolog:called_by/4.
prolog:called_by(on(G,_T), M, M, [G]). % why is this needed, given meta_goal(on(..))...?
prolog:called_by(because(G,_Why), M, M, [G]). % why is this needed, given meta_goal(on(..))...?
prolog:called_by(targetBody(G,_,_,_,_,_), M, M, [G]). 
%prolog:called_by(aggregate(_,G,_), M, M, [G]). % why is this needed, given meta_goal(on(..))...?

% does NOT fix the "G is not called" bug: prolog:called_by(mainGoal(G,_), M, M, [G]).


%%%% Support for automated tests/examples

run_examples :-
    forall(kp(M),(
        format("Knowledge page ~w~n",M),
        run_examples(M)
    )).

%TODO: 'true' assertions right now can have unknowns; this needs to be customizable
run_examples(Module) :-
    loaded_kp(Module),
    forall( catch(Module:example(Desc,Scenarios),error(existence_error(_, _), _),fail), (
        format(" Running example ~w~n",Desc),
        run_scenarios(Scenarios,Module,1,[],_U,_E)
    )).

%consider sequence of scenario fact sets; for now, a simple concatenation:
run_scenarios([scenario(Facts,G)|Scenarios],M,N,PreviousFacts,U,E) :- !,
    append(PreviousFacts,Facts,Facts_),
    query_once_with_facts(at(G,M),Facts_,U1,E1,Result),
    format("  Scenario ~w result   : ~w~n",[N,Result]),
    format("  Scenario ~w unknowns   : ~w~n",[N,U1]),
    format("  Scenario ~w explanation: ~w~n",[N,E1]),
    NewN is N+1,
    run_scenarios(Scenarios,M,NewN,Facts_,Un,En),
    append(U1,Un,U), append([E1],En,E).
run_scenarios([],_,_,_,[],[]).

% example_fact_sequence(+Module,?ExampleName,-Facts)
example_fact_sequence(M_,Name,Facts) :- 
    atom_string(M,M_),
    loaded_kp(M),
    (catch(M:example(Name,Scenarios),error(existence_error(_, _), _),fail) *-> true ; 
        (print_message(error,"Missing scenario for example: ~w"-[Name]), fail)),
    findall(SF,member(scenario(SF,_Assertion),Scenarios),Facts_),
    append(Facts_,Facts).


% once_with_facts(Goal,Module,AdditionalFacts,+DoUndo)
% Facts should be ground...
% asserts the facts (and deletes those with a - ) and calls Goal, stopping at the first solution, and optionally undoing the fact changes
% if a fact's predicate is undefined or not dynamic, it is declared (forever) as thread_local, 
% to support multiple clients
% BUG: not thread safe, failing to call thread_local(..) before
once_with_facts(G,M_,Facts,DoUndo) :-
    must_be(boolean,DoUndo),
    atom_string(M,M_),
    loaded_kp(M), % make sure the module is loaded
    assert_and_remember(Facts,M,from_with_facts,Undo),
    (true; DoUndo==true, once(Undo), fail),
    once(M:G),
    (DoUndo==true -> once(Undo) ; true).

% call_with_facts(+Goal,+Module,+AdditionalFacts) This does NOT undo the fact changes
call_with_facts(G,M_,Facts) :-
    atom_string(M,M_),
    loaded_kp(M), % make sure the module is loaded
    assert_and_remember(Facts,M,from_with_facts,_Undo),
    call(M:G).


% assert a list of timed facts, and returns a goal to undo the asserts
assert_and_remember([-Fact|Facts],M,Why,(Undo,Undos)) :- !,
    must_be(nonvar,Fact),
    assertion( \+ (functor(Fact,':-',_);functor(Fact,if,_)) ),
    canonic_fact_time(Fact,M,CF,Time,ExplicitTime), assert_and_remember_(delete,redefine,CF,ExplicitTime,_,Time,Why,Undo),
    assert_and_remember(Facts,M,Why,Undos).
assert_and_remember([Fact__|Facts],M,Why,(Undo,Undos)) :- must_be(nonvar,Fact__),
    (Fact__= (++(Fact_)) -> How=extend ; (Fact__=Fact_,How=redefine)),
    % Note: the following MUST be kept in sync with taxlog2prolog/3; essencially, this assumes no transform occurs:
    (Fact_ = if(Fact,Body) -> true ; (Fact=Fact_,Body=true)), %TODO: verify that rules are not functions etc
    canonic_fact_time(Fact,M,CF,Time,ExplicitTime), assert_and_remember_(add,How,CF,ExplicitTime,Body,Time,Why,Undo),
    assert_and_remember(Facts,M,Why,Undos).
assert_and_remember([],_,_,true).

assert_and_remember_(Operation,How,M:Fact,Explicit,Body,Time,Why,Undo) :- 
    assertion(How==extend;How==redefine),
   %TODO: Adds could check if there's a matching clause already, to avoid spurious facts at the end of some example runs
    % abolish caused 'No permission to modify thread_local_procedure'; weird interaction with yall.pl ..??
    % ( \+ predicate_property(M:Fact,_) -> (functor(Fact,F,N), thread_local(M:F/N)) ;
    %     predicate_property(M:Fact,(dynamic)) -> true ; 
    %     (functor(Fact,F,N), dynamic(M:F/N) ) % should be thread_local(M:F/N) !!!
    % ),
    % Instead of the above complications, we now use hypothetical_fact:
    % e.g. add a fact F in one scenarion and deleting in the next, which may leave F asserted when undoing the delete
    % this seems to require either using a variant test... or demanding facts to be ground
    % hypothetical_fact(M,H,Fact,Body_,Ref)
    functor(Fact,F,N), functor(Template,F,N), 
    Add = assert( hypothetical_fact(M,Template,Fact,targetBody(Body,Explicit,Time,'',Why,_),hypothetical,How) ),
    Delete = retractall( hypothetical_fact(M,Template,Fact,targetBody(Body,_Explicit,Time,'',Why,_),_,_) ),
    (Operation==add ->( Undo=Delete, Add) ; ( Undo=Add, Delete )).

% canonic_fact_time(+Fact,+DefaultModule,Module:Fact_,Time,-ExplicitTime)
canonic_fact_time(M_:on(F,T),_,M:F,T,true) :- !, atom_string(M,M_).
canonic_fact_time(M_:F,_,M:F,_,false) :- !, atom_string(M,M_).
canonic_fact_time(at(on(F,T),M),_,M_:F,T,true) :- !, atom_string(M_,M).
canonic_fact_time(at(F,M),_,M_:F,_,false) :- !, atom_string(M_,M).
canonic_fact_time(on(F,T),M,M:F,T,true) :- !.
canonic_fact_time(F,M,M:F,_,false).

%%%%% Explanations

expand_failure_trees_and_simp(E,FailedUnknowns,ES) :-
    expand_failure_trees(E,[],FailedUnknowns,Expanded), 
    simplify_explanation(Expanded,ES).

% expand_failure_trees(+Why,Unknowns,NewUnknowns,-ExpandedWhy)  the unknows are only those in failed branches
expand_failure_trees([s(X,M,Ref,Children)|Wn],U1,Un,[s(X,M,Ref,NewChildren)|EWn]) :- !, 
    expand_failure_trees(Children,U1,U2,NewChildren), expand_failure_trees(Wn,U2,Un,EWn).
expand_failure_trees([u(X,M,Ref,Children)|Wn],U1,Un,[u(X,M,Ref,NewChildren)|EWn]) :- !, 
    expand_failure_trees(Children,U1,U2,NewChildren), expand_failure_trees(Wn,U2,Un,EWn).
expand_failure_trees([f(ID,Module,CID,Children)|Wn],U1,Un,Expanded) :- 
    must_be(var,Children),
    findall(f(ChildID,M,ID,_),failed(ChildID,M,ID,_Cref,_ChildG),Children),
    expand_failure_trees(Children,U1,U2_,NewChildren_),
    ((NewChildren_==[],failed_success(ID,SU,Why)) -> ( % no failure suspects, but we have a (last) solution:
        append(U2_,SU,U2__), expand_failure_trees(Why,U2__,U2,NewChildren)) 
        ; (U2_=U2, NewChildren_ = NewChildren)  
        ),

    expand_failure_trees(Wn,U2,Un,EWn),
    (failed(ID,Module,CID,Cref,G) -> Expanded=[f(G,Module,Cref,NewChildren)|EWn]; append(NewChildren,EWn,Expanded)).
expand_failure_trees([],U,U,[]).

% simplify_explanation(+ExpandedWhy,-LeanerWhy)
simplify_explanation(Why,Simp) :- simplify_explanation(Why,[],_,Simp).

% simplify_explanation(+Why,+VisitedNodes,-NewVisitedNodes,-Simplified) ...Nodes are lists of (s/f/u)(Literal,Module)
simplify_explanation([E1|En],Visited,NewVisited,Simplified) :- E1=..[Type,X,M,Ref,Children], Node=..[Type,X,M],
    ((member(Node_,Visited), variant(Node_,Node))->
        simplify_explanation(En,Visited,NewVisited,Simplified) ;
        ( 
            simplify_explanation(Children,[Node|Visited],V2,SimpChildren), simplify_explanation(En,V2,NewVisited,SimpN), 
            E1Simp=..[Type,X,M,Ref,SimpChildren],
            Simplified=[E1Simp|SimpN]
            )
        ).
simplify_explanation([],V,V,[]).

% expand_explanation_refs(+ExpandedWhy,+ExtraFacts,-ExpandedRefLessWhy)
% TODO: recover original variable names? seems to require either some hacking with clause_info or reparsing
% transforms explanation: each nodetype(Literal,Module,ClauseRef,Children) --> nodetype(Literal,ClauseRef,Module,SourceString,OriginURL,Children)
expand_explanation_refs(CrudeE,Facts,taxlog(taxlogExplanation(E))) :- !,  
    expand_explanation_refs_taxlog(CrudeE, Facts, E). 

expand_explanation_refs(CrudeE,Facts,le(le_Explanation(E))) :-  !, 
    expand_explanation_refs_le(CrudeE, Facts, E). 
 
expand_explanation_refs(CrudeE,Facts,scasp(E)) :-
    expand_explanation_refs_casp(CrudeE, Facts, E). 

expand_explanation_refs_taxlog([Node|Nodes],Facts,[NewNode|NewNodes]) :- !,
    Node=..[Type,X,Module,Ref,Children], 
    refToSourceAndOrigin(Ref,Source,Origin),
    %TODO: is the following test against facts necessary???:
    ((member(XX,Facts), variant(XX,X)) -> NewOrigin=userFact ; NewOrigin=Origin),
    NewNode=..[Type,X,Ref,Module,Source,NewOrigin,NewChildren],
    expand_explanation_refs_taxlog(Children,Facts,NewChildren),
    expand_explanation_refs_taxlog(Nodes,Facts,NewNodes).
expand_explanation_refs_taxlog([],_,[]).

expand_explanation_refs_le([Node|Nodes],Facts, [NewNode|NewNodes]) :-  
    Node=..[Type,X0,Module,Ref,Children], 
    ( Children=[s(L,M2,Ref2,[])], unifiable(X0, L, _) ->  % to filter final leaves
        ( NextType = s, X = L, NextChildren = [], NextModule = M2, NextRef = Ref2 ) 
    ;   ( NextType = Type, X = X0, NextChildren = Children, NextModule = Module, NextRef = Ref)),
    refToSourceAndOrigin(NextRef,Source,Origin),
    %TODO: is the following test against facts necessary???:
    ((member(XX,Facts), variant(XX,X)) -> NewOrigin=userFact ; NewOrigin=Origin),
    (X\=[] -> 
        (translate_to_le(X, EnglishAnswer) -> 
            %print_message(informational, "Explaining ~w as ~w"-[X, EnglishAnswer])
            ( Output = EnglishAnswer            
            %NewNode=..[Type,Output,Ref,Module,Source,NewOrigin,NewChildren],
            %expand_explanation_refs(Children,Facts,NewChildren),
            %AllNodes = [NewNode|NewNodes]
            )
        ;   ( %print_message(informational, "Can't translate ~w"-[X]),
              term_string('Prolog Expression'(X), Output) )
        )
    ;         %AllNodes = NewNodes
        Output = 'it is a fact'
    ),
    %translate_to_le(X, Output),      
    NewNode=..[NextType,Output,NextRef,NextModule,Source,NewOrigin,NewChildren],
    expand_explanation_refs_le(NextChildren,Facts,NewChildren),
    expand_explanation_refs_le(Nodes,Facts,NewNodes).
expand_explanation_refs_le([],_,[]). 

translate_to_le(X, EnglishAnswer) :-
    le_input:translate_goal_into_LE(X, RawAnswer), le_input:name_as_atom(RawAnswer, EnglishAnswer). 
    %print_message(informational, "Translating ~w into ~w"-[X, EnglishAnswer]), !. 

expand_explanation_refs_casp([Node|Nodes],Facts,[X-NewChildren|NewNodes]) :- !,
    Node=..[_Type,X,_Module,_Ref,Children], 
    %refToSourceAndOrigin(Ref,Source,Origin),
    %TODO: is the following test against facts necessary???:
    %((member(XX,Facts), variant(XX,X)) -> NewOrigin=userFact ; NewOrigin=Origin),
    %NewNode=..[Type,X,Ref,Module,Source,NewOrigin,NewChildren],
    expand_explanation_refs_casp(Children,Facts,NewChildren),
    expand_explanation_refs_casp(Nodes,Facts,NewNodes).
expand_explanation_refs_casp([],_,[]).

% [s(a(1,a),<clause>(0x7f95c763bc30),[s(c(1),<clause>(0x7f95c763bd90),[]),s(t(a),<clause>(0x7f95c763c000),[])])]
explanation_node_type(s,success).
explanation_node_type(f,failure).
explanation_node_type(u,unknown). % a success depending on unknown subgoals

% for HTML rendering, see explanation_renderer.pl

/* Graphviz support, not very promising given the large size of our labels (predicate names)
% experimental; would need unique IDs to avoid large term duplication
explanationChild(s(_,_Ref,Children),C) :- member(C,Children).

explanationRelation(Root,Parent,Child) :- Parent=Root, explanationChild(Parent,Child).
explanationRelation(Root,Parent,Child) :- explanationChild(Root,X), explanationRelation(X,Parent,Child).

explanationGraph(E,dot(digraph([rankdir='TB'|Items]))) :-
    setof(edge(From->To,[label=""]), E_^From^To^(member(E_,E), explanationRelation(E_,From,To)), Edges),
    setof(node(N,NodeAttrs), Attrs^From^To^(member(edge(From->To,Attrs),Edges), (From=N;To=N), nodeAttributes(N,NodeAttrs)), Nodes),
    append(Edges,Nodes,Items).

nodeAttributes(s(G,_Ref,_),[label=S]) :- format(string(S),"~w",G).
nodeAttributes(unknown(at(G,K)), [label=S]) :- format(string(S),"~w",G).
nodeAttributes(failed(at(G,K)), [color=red,label=S]) :- format(string(S),"~w",G).
nodeAttributes(at(G,K), [color=green,label=S]) :- format(string(S),"~w",G).
*/

%%%% Common background knowledge, probably to go elsewhere:

%Time predicates; they assume times are atoms in iso_8601 format

%!  after(+Later,+Earlier) is det.
%   Arguments must be dates in iso_8601 format, e.g. '20210206' or '2021-02-06T08:25:34'
isafter(Later,Earlier) :- 
    parse_time(Later,L), parse_time(Earlier,E), L>E.
is_not_before(Later,Earlier) :-
    parse_time(Later,L), parse_time(Earlier,E), L>=E.
isbefore(Earlier,Later) :-
    parse_time(Later,L), parse_time(Earlier,E), E<L, !.
% an argument can be any number
isbefore(Earlier,Later) :-
    number(Later), number(Earlier), Earlier<Later.

% Dates in seconds since 1970-01-01T00:00:00, Day in partial format Year-Month-Day
% set_time_of_day(+Day, +Hour_Min_Secs, -DateInSeconds)
set_time_of_day(Day, Hour_Min_Secs, DateInSeconds) :- atom(Day), 
    concat_atom([Day, 'T', Hour_Min_Secs], '', Date), parse_time(Date,DateInSeconds). 

start_of_day(Day, Date) :-
    set_time_of_day(Day, '00:00:00', Date).
end_of_day(Day, Date) :-
    set_time_of_day(Day, '23:59:59', Date).

unparse_time(Stamp, Date) :-
    stamp_date_time(Stamp, DateTime, 'UTC'), 
    %DateTime = date(Y,M,D,H,Mn,S,Off,TZ,DST)
    DateTime = date(Year,Month,Day,Hours,Minutes,Seconds,_,_,_), 
    concat_atom([Year,'-', Month, '-', Day,'T', Hours,':', Minutes, ':', Seconds], '', Date). 

%le_to_taxlog:dict([is_1_day_after, Later, Before],
%        [date-date, second_date-date],
%        [Later, is, '1', day, after, Before]).
is_1_day_after(Later, Before) :-
    is_days_after(Later, 1, Before).
%le_to_taxlog:dict([is_days_after, A, B, C],
%        [date-date, number-number, second_date-date],
%        [A, is, B, days, after, C]).
is_days_after(Later, Count, Before) :-
    nonvar(Before), nonvar(Count), !, 
    Later is Count*86400 + Before. 
is_days_after(Later, Count, Before) :-
    nonvar(Later), nonvar(Count), !, 
    Before is Later - Count*86400. 
is_days_after(Later, Count, Before) :-
    nonvar(Later), nonvar(Before),
    Count is round(Later - Before) div 86400. % using negative number to indicate reserve order 

%! immediately_before(?Earlier,?Later) is det.
%  Later is 24h after Earlier; at least one must be known
immediately_before(Earlier,Later) :- 
    ((nonvar(Earlier);nonvar(Later)) -> true ; throw("Unbound arguments in immediately_before"-[])),
    (nonvar(Earlier) -> (parse_time(Earlier,E), L is E+24*3600 ) ; true),
    (nonvar(Later) -> (parse_time(Later,L), E is L-24*3600) ; true),
    (var(Earlier) -> format_time(string(Earlier),"%FT%T%z",E) ; true),
    (var(Later) ->  format_time(string(Later),"%FT%T%z",L) ; true).

same_date(T1,T2) :- 
    format_time(string(S),"%F",T1), format_time(string(S),"%F",T2).

%! subtract_days(+LaterDate,+EarlierDate,-Days)
%  How many days (24 hours intervals) later
subtract_days(LaterDate,EarlierDate,Days) :-
    parse_time(LaterDate,Later), parse_time(EarlierDate,Earlier),
    Days is round(Later-Earlier) div (24*3600).

%! this_year(?Year) is det.
%  The current year
this_year(Y) :- get_time(Now), stamp_date_time(Now,date(Y,_M,_D,_,_,_,_,_,_),local).

%! uk_tax_year(?DateInTaxYear,?FirstYear,-StartDate,-EndDate)
%  "the range of uk tax year Y is from Start to End"
%  Given either a Date or a number for the first year, returns a tax year date range
uk_tax_year(D,FirstYear,Start,End) :- nonvar(D), !, FirstYear=StartYear,
    parse_time(D,Time), stamp_date_time(Time,DT,local), DT=..[date,Year,Month,Day|_],
    ((Month>4;Month=4,Day>=6) -> StartYear = Year ; StartYear is Year-1),
    EndYear is StartYear+1,
    format_time(string(Start),"%F",date(StartYear,4,6)),
    format_time(string(End),"%F",date(EndYear,4,5)).
uk_tax_year(Start,StartYear,Start,End) :- must_be(integer,StartYear),
    assertion(StartYear>1899), % format_time limitation
    EndYear is StartYear+1,
    format_time(string(Start),"%F",date(StartYear,4,6)),
    format_time(string(End),"%F",date(EndYear,4,5)).


%! in(X,List) is nondet.
%  X is in List
in(X,List) :- must_be(list,List), member(X,List).

has_as_head_before([Head|Rest],Head,Rest). 

:- if(current_module(swish)). %%%%% On SWISH:

sandbox:safe_primitive(reasoner:query(_,_,_,_)).
sandbox:safe_primitive(reasoner:query_once_with_facts(_,_,_,_,_)).
sandbox:safe_primitive(reasoner:query_with_facts(_,_,_,_,_)).
sandbox:safe_primitive(reasoner:render_questions(_,_)).


:- use_module(swish(lib/html_output),[html/1]). 
% hack to avoid SWISH errors:
myhtml(H) :-  this_capsule(SwishModule), SwishModule:html(H).

kbModule(M) :- this_capsule(M).


:- else. % On command-line SWI-Prolog, no user restrictions:

kbModule(user).
:- endif.
