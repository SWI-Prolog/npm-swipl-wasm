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

:- module(_,[
    op(1195,fx, user:(++)), % for hypothetical facts and rules 
    op(1190,xfx,user:(if)),
    op(1187,xfx,user:(then)),
    op(1187,xfx,user:(must)),
    op(1185,fx,user:(if)),
    op(1185,xfy,user:else),
    op(1000,xfy,user:and), % same as ,
    op(1050,xfy,user:or), % same as ;
    op(900,fx,user:not), % same as \+
    op(700,xfx,user:in),
    op(600,xfx,user:on),
    op(1150,xfx,user:because), % to support because(on(p,t),why) if ...
    op(700,xfx,user:at), % note vs. negation...incompatible with LPS fluents
    % date operators
    op(700,xfx,user:is_not_before),
    op(700,xfx,user:before),
    op(700,xfx,user:after),
    taxlog2prolog/3,
    semantics2prolog/3,
    semantics2prolog2/3,
    current_source/1
    ]).

:- use_module(kp_loader,[kp_location/3,my_xref_defined/3]).

:- use_module(library(prolog_xref)).
:- use_module(library(prolog_colour)).
:- use_module(library(pengines)).


:- if(current_module(swish)). 
:- use_module('le_swish.pl'). % module to handle the gitty filesystem
:- else.
:- use_module('le_local.pl'). % module to handle the local filesystem 
:- endif.


/*
Transforms source rules into our "no time on heads" representation, using a body wrapper to carry extra information:
    targetBody(RealBody,HasTimeOnHead,Time,URL,Why,LE_line or taxlog) % i.e. the line number in LE or the label taxlog

P on T if Body  -->  P :- targetBody(Body,true,T,'',[],LE_line or taxlog)
P on T because Why :- PrologBody   -->   P :- targetBody(PrologBody,true,T,'',Why,LE_line or taxlog)
P if Body  --> P  :- targetBody(Body,false,_,'',[],LE_line or taxlog)
Admissible variants with a specific URL:
P on T at URL if Body --> P :- targetBody(Body,true,T,URL,[],LE_line or taxlog)
P at URL if Body  -->  P :- targetBody(Body,false,_,URL,[],LE_line or taxlog)
*/

semantics2prolog2(if(N,H,B),neck(if)-[],(H:-targetBody(B,false,_,'',[],NN))) :- !, % working rule with line number
    NN is N + 3. % correction to linecount
    %taxlogHeadSpec(H,SpecH), taxlogBodySpec(B,SpecB).
semantics2prolog2(if(H,B),neck(if)-[],(H:-targetBody(B,false,_,'',[],3))) :- !. % pre-settings without line numbers
    %taxlogHeadSpec(H,SpecH), taxlogBodySpec(B,SpecB),
    %this_capsule(SwishModule),
    %declare_facts_as_dynamic(SwishModule, [H]). 
%semantics2prolog2(if(H,B),neck(if)-[SpecH,SpecB],(H:-B)) :- !,
%    SpecH=classify, SpecB=classify. 
    %taxlogHeadSpec(H,SpecH), taxlogBodySpec(B,SpecB).
%semantics2prolog2(mainGoal(G,Description),delimiter-[Spec,classify],(mainGoal(G,Description):-(_=1->true;GG))) :- !, % hack to avoid 'unreferenced' highlight in SWISH
%    functor(G,F,N), functor(GG,F,N), % avoid "Singleton-marked variable appears more than once"
%    taxlogBodySpec(G,Spec).
semantics2prolog2(abducible(Abd,Body),delimiter-[classify,classify],abducible(Abd,Body)) :- !. 
    % this_capsule(SwishModule),
    % declare_facts_as_dynamic(SwishModule, [abducible(_,_)]), !. 
semantics2prolog2(example(T,Sequence),delimiter-[classify,Spec],example(T,Sequence)) :- !, 
    % this_capsule(SwishModule),
    % declare_facts_as_dynamic(SwishModule, [example(_,_)]), !, 
    Spec = classify. % just a hack - scenarioSequenceSpec must be different for prolog's scenarios
    %(Sequence==[]->Spec=classify ; (Spec=list-SeqSpec, scenarioSequenceSpec(Sequence,SeqSpec))).
semantics2prolog2(query(Name,Goal),delimiter-[classify,classify],query(Name,Goal)) :- !.
    % this_capsule(SwishModule),
    % declare_facts_as_dynamic(SwishModule, [query(_,_)]), !. 
semantics2prolog2(metapredicates(Assumptions), delimiter-[classify,classify],metapredicates([N])) :- !,
    % this_capsule(SwishModule), 
    lists:length(Assumptions,N).
    % declare_facts_as_dynamic(SwishModule, Assumptions), !. 
semantics2prolog2(predicates(Assumptions), delimiter-[classify,classify],predicates([N])) :- !,
    % this_capsule(SwishModule), 
    lists:length(Assumptions,N).
    % declare_facts_as_dynamic(SwishModule, Assumptions), !. 
    %print_message(informational, "asserted: ~w"-[Assumptions]).
semantics2prolog2(events(Assumptions), delimiter-[classify,classify],events([N])) :- !, 
    % this_capsule(SwishModule), 
    lists:length(Assumptions,N). 
    % declare_facts_as_dynamic(SwishModule, [happens(_,_), initiates(_,_,_), terminates(_,_,_)|Assumptions]), !.
semantics2prolog2(fluents(Assumptions), delimiter-[classify,classify],fluents([N])) :- !, 
    % this_capsule(SwishModule), 
    lists:length(Assumptions,N). 
    % declare_facts_as_dynamic(SwishModule, [it_is_illegal(_,_)|Assumptions]), !.
semantics2prolog2(target(T), delimiter-[classify,classify],target(T)). 
    % this_capsule(SwishModule), 
    % declare_facts_as_dynamic(SwishModule, [just_saved_scasp(_, _)]), !. 

taxlog2prolog(if(_LineNumber,H,B), Spec, New) :- !, taxlog2prolog(if(H,B),Spec,New). % hack for LogicalEnglish
taxlog2prolog(if(function(Call,Result),Body), neck(if)-[delimiter-[head(meta,Call),classify],SpecB], (function(Call,Result):-Body)) :- !,
    taxlogBodySpec(Body,SpecB).
taxlog2prolog(if(at(on(H,T),Url),B), neck(if)-[delimiter-[delimiter-[SpecH,classify],classify],SpecB], (H:-targetBody(B,true,T,Url,[],taxlog))) :- !,
    taxlogHeadSpec(H,SpecH), taxlogBodySpec(B,SpecB).
taxlog2prolog(if(at(H,Url),B), neck(if)-[delimiter-[SpecH,classify],SpecB], (H:-targetBody(B,false,_T,Url,[],taxlog))) :- !,
    taxlogHeadSpec(H,SpecH), taxlogBodySpec(B,SpecB).
taxlog2prolog(if(on(H,T),B), neck(if)-[delimiter-[SpecH,classify],SpecB], (H:-targetBody(B,true,T,'',[],taxlog))) :- !,
    taxlogHeadSpec(H,SpecH), taxlogBodySpec(B,SpecB).
taxlog2prolog(if(H,B),neck(if)-[SpecH,SpecB],(H:-targetBody(B,false,_,'',[],taxlog))) :- !,
    taxlogHeadSpec(H,SpecH), taxlogBodySpec(B,SpecB).
taxlog2prolog((because(on(H,T),Why):-B), neck(clause)-[ delimiter-[delimiter-[SpecH,classify],classify], SpecB ], (H:-targetBody(call(B),true,T,'',Why,taxlog))) :- Why\==[], !,
    taxlogHeadSpec(H,SpecH), taxlogBodySpec(B,SpecB).
taxlog2prolog(mainGoal(G,Description),delimiter-[Spec,classify],(mainGoal(G,Description):-(_=1->true;GG))) :- !, % hack to avoid 'unreferenced' highlight in SWISH
    functor(G,F,N), functor(GG,F,N), % avoid "Singleton-marked variable appears more than once"
    taxlogBodySpec(G,Spec).
taxlog2prolog((example(T,Sequence):-Body), neck(clause)-[delimiter-[classify,Spec],classify],(example(T,Sequence):-Body)) :- !,  
    (Sequence==[]->Spec=classify ; (Spec=list-SeqSpec, scenarioSequenceSpec(Sequence,SeqSpec))).
taxlog2prolog(example(T,Sequence),delimiter-[classify,Spec],example(T,Sequence)) :- !,  
    (Sequence==[]->Spec=classify ; (Spec=list-SeqSpec, scenarioSequenceSpec(Sequence,SeqSpec))).
taxlog2prolog(question(X,QuestionTerm),delimiter-[classify,classify],question(X,QuestionTerm)) :- !.
taxlog2prolog(question(X,QuestionTerm,Answer),delimiter-[classify,classify,classify],question(X,QuestionTerm,Answer)) :- !.
taxlog2prolog(irrelevant_explanation(G),delimiter-[Spec],irrelevant_explanation(G)) :- !, 
    taxlogBodySpec(G,Spec).
taxlog2prolog(query(Name,Goal),delimiter-[classify,classify],query(Name,Goal)).

% extending to cover new structural changes at semantical level

semantics2prolog(if(N,H,B),neck(if)-[SpecH,SpecB],(H:-targetBody(B,false,_,'',[],NN))) :- !, % working rule with line number
    NN is N + 3, % correction to linecount
    taxlogHeadSpec(H,SpecH), taxlogBodySpec(B,SpecB).
semantics2prolog(if(H,B),neck(if)-[SpecH,SpecB],(H:-targetBody(B,false,_,'',[],3))) :- !, % pre-settings without line numbers
    taxlogHeadSpec(H,SpecH), taxlogBodySpec(B,SpecB),
    this_capsule(SwishModule),
    declare_facts_as_dynamic(SwishModule, [H]). 
%semantics2prolog(if(H,B),neck(if)-[SpecH,SpecB],(H:-B)) :- !,
%    SpecH=classify, SpecB=classify. 
    %taxlogHeadSpec(H,SpecH), taxlogBodySpec(B,SpecB).
%semantics2prolog(mainGoal(G,Description),delimiter-[Spec,classify],(mainGoal(G,Description):-(_=1->true;GG))) :- !, % hack to avoid 'unreferenced' highlight in SWISH
%    functor(G,F,N), functor(GG,F,N), % avoid "Singleton-marked variable appears more than once"
%    taxlogBodySpec(G,Spec).
semantics2prolog(abducible(Abd,Body),delimiter-[classify,classify],abducible(Abd,Body)) :-
    this_capsule(SwishModule),
    declare_facts_as_dynamic(SwishModule, [abducible(_,_)]), !. 
semantics2prolog(example(T,Sequence),delimiter-[classify,Spec],example(T,Sequence)) :-  
    this_capsule(SwishModule),
    declare_facts_as_dynamic(SwishModule, [example(_,_)]), !, 
    Spec = classify. % just a hack - scenarioSequenceSpec must be different for prolog's scenarios
    %(Sequence==[]->Spec=classify ; (Spec=list-SeqSpec, scenarioSequenceSpec(Sequence,SeqSpec))).
semantics2prolog(query(Name,Goal),delimiter-[classify,classify],query(Name,Goal)) :-
    this_capsule(SwishModule),
    declare_facts_as_dynamic(SwishModule, [query(_,_)]), !. 
semantics2prolog(metapredicates(Assumptions), delimiter-[classify,classify],metapredicates([N])) :- 
    this_capsule(SwishModule), lists:length(Assumptions,N),
    declare_facts_as_dynamic(SwishModule, Assumptions), !. 
semantics2prolog(predicates(Assumptions), delimiter-[classify,classify],predicates([N])) :- 
    this_capsule(SwishModule), lists:length(Assumptions,N),
    declare_facts_as_dynamic(SwishModule, Assumptions), !. 
    %print_message(informational, "asserted: ~w"-[Assumptions]).
semantics2prolog(events(Assumptions), delimiter-[classify,classify],events([N])) :- 
    this_capsule(SwishModule), lists:length(Assumptions,N),
    declare_facts_as_dynamic(SwishModule, [happens(_,_), initiates(_,_,_), terminates(_,_,_)|Assumptions]), !.
semantics2prolog(fluents(Assumptions), delimiter-[classify,classify],fluents([N])) :-
    this_capsule(SwishModule), lists:length(Assumptions,N),
    declare_facts_as_dynamic(SwishModule, [it_is_illegal(_,_)|Assumptions]), !.
semantics2prolog(target(T), delimiter-[classify,classify],target(T)) :- 
    this_capsule(SwishModule), 
    declare_facts_as_dynamic(SwishModule, [just_saved_scasp(_, _)]), !. 

% assuming one example -> one scenario -> one list of facts. % deprecated
% declare_dynamic(Module, [scenario(Facts, _)]) :- declare_facts_as_dynamic(Module, Facts).

declare_facts_as_dynamic(_, []) :- !. 
declare_facts_as_dynamic(M, [F|R]) :- functor(F, P, A),  % facts are the templates now
    dynamic([M:P/A], [thread(local), discontiguous(true)]), declare_facts_as_dynamic(M, R). 

% note: keep the above cases coherent with kp_loader:system_predicate/1

scenarioSequenceSpec([S|Scenarios],[Spec|Specs]) :- !,
    scenarioSpec(S,Spec),
    scenarioSequenceSpec(Scenarios,Specs).
scenarioSequenceSpec([],[]).

scenarioSpec(scenario(Facts,Assertion),delimiter-[FactsSpec,Spec]) :- 
    (Facts==[] -> FactsSpec=classify ; (factsSpecs(Facts,FS), FactsSpec=list-FS)),
    taxlogBodySpec(Assertion,Spec).

factsSpecs(Facts,classify) :- var(Facts), !.
factsSpecs([Fact_|Facts],[FactSpec|Specs]) :- !,  
    (Fact_= -Fact -> FactSpec= delimiter-[FS] ; Fact_= '++'(Fact) -> FactSpec= delimiter-[FS]; (Fact=Fact_,FactSpec=FS)),
    nonvar(Fact),
    (Fact=if(H,B)->(
        taxlogHeadSpec(H,FSH),taxlogBodySpec(B,FSB),FS=neck(if)-[FSH,FSB]);
        taxlogHeadSpec(Fact,FS) ), 
    factsSpecs(Facts,Specs).
factsSpecs([],[]).

taxlogHeadSpec(H,head(Class, H)) :- current_source(UUID),
    !,
    xref_module(UUID,Me),
    (H=on(RealH,_T)->true;H=RealH),
    (xref_called(_Other,Me:RealH, _) -> (Class=exported) ;
        xref_called(UUID, RealH, _By) -> (Class=head) ;
        Class=unreferenced).
taxlogHeadSpec(H,head(head, H)).

:- multifile swish_highlight:style/3.
swish_highlight:style(neck(if),     neck, [ text(if) ]).

% :- thread_local current_module/1.
% :- multifile prolog_colour:directive_colours/2.
% prolog_colour:directive_colours((:- module(M,_)),null) :-
%     mylog(detected_module/M), % NOT CALLED AT ALL???
%     retractall(current_module(_)), assert(current_module(M)), fail.


% this must be in sync with the interpreter i(...) and prolog:meta_goal(...) hooks
taxlogBodySpec(V,classify) :- var(V), !.
taxlogBodySpec(and(A,B),delimiter-[SpecA,SpecB]) :- !, 
    taxlogBodySpec(A,SpecA), taxlogBodySpec(B,SpecB).
taxlogBodySpec((A,B),delimiter-[SpecA,SpecB]) :- !, 
    taxlogBodySpec(A,SpecA), taxlogBodySpec(B,SpecB).
taxlogBodySpec(or(A,B),delimiter-[SpecA,SpecB]) :- !, 
    taxlogBodySpec(A,SpecA), taxlogBodySpec(B,SpecB).
taxlogBodySpec((A;B),delimiter-[SpecA,SpecB]) :- !, 
    taxlogBodySpec(A,SpecA), taxlogBodySpec(B,SpecB).
taxlogBodySpec(must(if(I),M),delimiter-[delimiter-SpecI,SpecM]) :- !, 
    taxlogBodySpec(I,SpecI), taxlogBodySpec(M,SpecM).
taxlogBodySpec(not(G),delimiter-[Spec]) :- !, 
    taxlogBodySpec(G,Spec).
taxlogBodySpec((\+G),delimiter-[Spec]) :- !, 
    taxlogBodySpec(G,Spec).
taxlogBodySpec(then(if(C),else(T,Else)),delimiter-[delimiter-[SpecC],delimiter-[SpecT,SpecE]]) :- !, 
    taxlogBodySpec(C,SpecC), taxlogBodySpec(T,SpecT), taxlogBodySpec(Else,SpecE).
taxlogBodySpec(then(if(C),Then),delimiter-[delimiter-[SpecC],SpecT]) :- !, 
    taxlogBodySpec(C,SpecC), taxlogBodySpec(Then,SpecT).
taxlogBodySpec(forall(C,Must),control-[SpecC,SpecMust]) :- !, 
    taxlogBodySpec(C,SpecC), taxlogBodySpec(Must,SpecMust).
taxlogBodySpec(setof(_X,G,_L),control-[classify,SpecG,classify]) :- !, 
    taxlogBodySpec(G,SpecG). 
taxlogBodySpec(bagof(_X,G,_L),control-[classify,SpecG,classify]) :- !, 
    taxlogBodySpec(G,SpecG). 
taxlogBodySpec(_^G,delimiter-[classify,SpecG]) :- !,
    taxlogBodySpec(G,SpecG).
% this is needed only to deal with multiline instances of aggregate... (or of any predicate of our own colouring, apparently:-( )
taxlogBodySpec(aggregate(_X,G,_L),control-[classify,SpecG,classify]) :- !, 
    taxlogBodySpec(G,SpecG). 
taxlogBodySpec(aggregate_all(_X,G,_L),control-[classify,SpecG,classify]) :- !, 
    taxlogBodySpec(G,SpecG). 
taxlogBodySpec(findall(_X,G,_L),control-[classify,SpecG,classify]) :- !, 
    taxlogBodySpec(G,SpecG). 
% questions are no longer goals, just annotations for (rendering unknown) goal literals
%taxlogBodySpec(question(_,_),delimiter-[classify,classify]). % to avoid multiline colouring bug
%taxlogBodySpec(question(_),delimiter-[classify]).
taxlogBodySpec(M:G,delimiter-[classify,SpecG]) :- !, taxlogBodySpec(at(G,M),delimiter-[SpecG,classify]).
taxlogBodySpec(at(G_,M_),Spec) :- nonvar(M_), nonvar(G_), !, % assuming atomic goals
    atom_string(M,M_), %TODO: this might be cleaned up/refactored with the next clauses:
    (G_=on(G,_) -> Spec=delimiter-[delimiter-[SpecG,classify],classify]; (G=G_, Spec=delimiter-[SpecG,classify])),
    (my_xref_defined(M,G,_)-> SpecG=goal(imported(M),G)-classify ; SpecG=goal(undefined,G)-classify).
taxlogBodySpec(on(G,_T),delimiter-[SpecG,classify] ) :- !,
    taxlogBodySpec(G,SpecG).
taxlogBodySpec(G,Spec) :-  
    (compound(G)->Spec=goal(Class,G)-classify;Spec=goal(Class,G)), 
    current_source(UUID), taxlogGoalSpec(G, UUID, Class),
    !. 
taxlogBodySpec(_G,classify).

taxlogGoalSpec(G, UUID, Class) :-
    (my_xref_defined(UUID, G, Class) -> true ; 
        %prolog_colour:built_in_predicate(G)->Class=built_in ;
        my_goal_classification(G,Class) -> true;
        Class=undefined).

:- if(current_prolog_flag(version_data,swi(8, 2, _, _))).
my_goal_classification(G,Class) :-
    prolog_colour:call_goal_classification(G, Class).
:- elif(( current_prolog_flag(version_data,V), V@>= swi(8, 3, 0, []))).
my_goal_classification(G,Class) :-
    prolog_colour:call_goal_classification(G, _Module, Class).
:- else.
:- print_message(error,"You need SWI-Prolog 8.2 or later"-[]), halt(1).
:- endif.

:- if(current_module(swish)). %%% only when running with the SWISH web server:
% hack to find the editor (e.g. its module name) that triggered the present highlighting
current_source(UUID) :- 
    swish_highlight:current_editor(UUID, _TB, source, Lock, _), mutex_property(Lock,status(locked(_Owner, _Count))), !.
current_source(UUID) :- 
    %mylog('Could not find locked editor, going with the first one'), 
    swish_highlight:current_editor(UUID, _TB, source, _Lock, _), !.

:- else. %% barebones SWI-Prolog:
% find the module in the file being coloured (which has been xref'd already)
current_source(Source) :- 
    prolog_load_context(source,File), kp_location(Source,File,false).
:- endif.

