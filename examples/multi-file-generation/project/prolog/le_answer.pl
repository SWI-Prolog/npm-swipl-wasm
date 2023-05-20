/* le_answer: a prolog module with predicates to handle queries in Logical English

   http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

Main predicate: answer/1, /2, /3, /4

which can be used on the new command interface of LE on SWISH
(e.g. answer/1 and others querying predicates):

? answer("query one with scenario test"). 

*/

:- module(le_answer, 
    [le_taxlog_translate/4, 
    translate_goal_into_LE/2, 
    op(1000,xfy,user:and),  % to support querying
    op(800,fx,user:resolve), % to support querying
    op(800,fx,user:answer), % to support querying
    op(800,fx,user:répondre), % to support querying in french
    op(850,xfx,user:with), % to support querying
    op(850,xfx,user:avec), % to support querying in french
    op(800,fx,user:risposta), % to support querying in italian
    op(850,xfx,user:con), % to support querying in italian
    op(800,fx,user:responde), % to support querying in spanish
    %op(1150,fx,user:show), % to support querying
    op(850,xfx,user:of), % to support querying
    %op(850,fx,user:'#pred'), % to support scasp 
    %op(800,xfx,user:'::'), % to support scasp 
    op(950, xfx, ::),           % pred not x :: "...".
    op(1200, fx, #),
    op(1150, fx, pred),
    op(1150, fx, show),
    op(1150, fx, abducible),
    dump/4, dump/3, dump/2, dump_scasp/3, split_module_name/3,
    prepare_query/6, assert_facts/2, retract_facts/2, parse_and_query/5, parse_and_query_and_explanation/5,
    le_expanded_terms/2, show/1, source_lang/1, targetBody/6
    ]).

%:- use_module(library(sandbox)).
:- use_module(library(pengines_sandbox)). 

% required for sCASP justification (from ~/git/swish/pack/sCASP/examples)

% :- use_module(library(scasp)).
% :- use_module(library(scasp/html)).
% :- use_module(library(scasp/output)).
% :- use_module(library(scasp/json)).

% :- use_module(library(http/http_server)).
% :- use_module(library(http/html_write)).
% :- use_module(library(http/js_write)).
% :- use_module(library(http/html_head)).
% :- use_module(library(http/http_path)).
% :- use_module(library(http/http_error)).
% :- use_module(library(http/jquery)).
% :- use_module(library(http/http_dispatch)).
% :- use_module(library(dcg/high_order)).
% :- use_module(library(http/term_html)).
% :- use_module(library(http/http_json)).
% :- use_module(library(http/http_client)).
% :- use_module(library(http/http_host)).

%:- multifile sandbox:safe_primitive/1.
%:- multifile sandbox:safe_meta/2.

:- use_module('le_input.pl').  
:- use_module('syntax.pl').
:- use_module('api.pl'). 
:- use_module('reasoner.pl'). 
:- use_module('./tokenize/prolog/tokenize.pl').


% html libs
:- use_module(library(http/html_write)).
:- use_module(library(http/term_html)).
:- use_module(library(http/js_write)).

:- multifile http:location/3.
:- dynamic   http:location/3.

% Does justification tree needs this?

%http:location(scasp, root(scasp), []).
%http:location(js,    scasp(js),   []).
%http:location(css,   scasp(css),  []).
    
:- discontiguous statement/3, declaration/4, _:example/2, _:query/2, _:is_/2. 

/* ---------------------------------------------------------------  meta predicates CLI */

is_it_illegal(English, Scenario) :- % only event as possibly illegal for the time being
    (le_input:parsed -> true; fail), !, 
    translate_query(English, happens(Goal, T)), % later -->, Kbs),
    %print_message(informational, "Goal Name: ~w"-[GoalName]),predef_
    this_capsule(SwishModule), %SwishModule:query(GoalName, Goal), 
    %extract_goal_command(Question, SwishModule, Goal, Command), 
    %copy_term(Goal, CopyOfGoal), 
    %translate_goal_into_LE(CopyOfGoal, RawGoal),  name_as_atom(RawGoal, EnglishQuestion), 
    %print_message(informational, "Testing illegality: ~w"-[EnglishQuestion]),
    %print_message(informational, "Scenario: ~w"-[Scenario]),
    get_assumptions_from_scenario(Scenario, SwishModule, Assumptions), 
    setup_call_catcher_cleanup(assert_facts(SwishModule, Assumptions), 
            %catch(SwishModule:holds(Goal), Error, ( print_message(error, Error), fail ) ), 
            %catch(Command, Error, ( print_message(error, Error), fail ) ), 
            catch(SwishModule:it_is_illegal(Goal, T), Error, ( print_message(error, Error), fail ) ), 
            _Result, 
            retract_facts(SwishModule, Assumptions)), 
    translate_goal_into_LE(Goal, RawAnswer), name_as_atom(RawAnswer, EnglishAnswer),  
    print_message(informational, "Answers: ~w"-[EnglishAnswer]).

% extract_goal_command/4
% extract_goal_command(WrappedGoal, Module, InnerGoal, RealGoal)
extract_goal_command(Goal, M, InnerGoal, Command) :- nonvar(Goal), 
    extract_goal_command_(Goal, M, InnerGoal, Command). 

extract_goal_command_((A;B), M, (IA;IB), (CA;CB)) :-
    extract_goal_command_(A, M, IA, CA), extract_goal_command_(B, M, IB, CB), !. 
extract_goal_command_((A,B), M, (IA,IB), (CA,CB)) :-
    extract_goal_command_(A, M, IA, CA), extract_goal_command_(B, M, IB, CB), !. 
extract_goal_command_(holds(Goal,T), M, Goal, (holds(Goal,T);M:holds(Goal,T))) :- !.
extract_goal_command_(happens(Goal,T), M, Goal, (happens(Goal,T);M:happens(Goal,T))) :- !.
extract_goal_command_(Goal, M, Goal, M:Goal) :- !. 

get_assumptions_from_scenario(noscenario, _, []) :- !.  
get_assumptions_from_scenario(Scenario, SwishModule, Assumptions) :-
    SwishModule:example(Scenario, [scenario(Assumptions, _)]), !.

translate_query(English_String, Goals) :-
    tokenize(English_String, Tokens, [cased(true), spaces(true), numbers(false)]),
    unpack_tokens(Tokens, UTokens), 
    clean_comments(UTokens, CTokens), 
    phrase(conditions(0, [], _, Goals), CTokens) -> true 
    ; ( error_notice(error, Me,Pos, ContextTokens), print_message(error, [Me,Pos,ContextTokens]), fail ). 

/* ----------------------------------------------------------------- Event Calculus  */
% holds/2
holds(Fluent, T) :-
    this_capsule(SwishModule), %trace, 
    SwishModule:happens(Event, T1), 
    rbefore(T1,T),  
    SwishModule:initiates(Event, Fluent, T1), 
    %(nonvar(T) -> rbefore(T1,T); T=(after(T1)-_)),  % T1 is strictly before T 'cos T is not a variable
    %(nonvar(T) -> rbefore(T1,T); true),
    not(interrupted(T1, Fluent, T)).

rbefore(T1, T) :-
    nonvar(T1), nonvar(T), isbefore(T1, T). %, !.
%rbefore(T1, T) :- (var(T1); var(T)), !. % if anyone is a variable, don't compute
%rbefore(T1, (after(T2)-_)) :-
%    nonvar(T1), nonvar(T2), before(T1, T2).

% interrupted/3
interrupted(T1, Fluent, T2) :- %trace, 
    this_capsule(SwishModule),
    SwishModule:happens(Event, T), 
    rbefore(T, T2), 
    SwishModule:terminates(Event, Fluent, T), 
    (rbefore(T1, T); T1=T), !.
    %(nonvar(T2) -> rbefore(T, T2) ; true ), !.  
    %(T2=(after(T1)-_)->T2=(after(T1)-before(T)); rbefore(T,T2)). 

/* ----------------------------------------------------------------- CLI English */
% answer/1
% answer(+Query or Query Expression)
answer(English) :- %trace, 
    answer(English, empty). 

% answer/2
% answer(+Query, with(+Scenario))
answer(English, Arg) :- %trace, 
    le_input:parsed,
    prepare_query(English, Arg, SwishModule, Goal, Facts, Command), 
    ((SwishModule:just_saved_scasp(FileName, ModuleName), FileName\=null) -> 
        %print_message(informational, "To query file ~w in module ~w "-[FileName, ModuleName]),
        load_file_module(FileName, ModuleName, true), 
        %print_message(informational, "loaded scasp ~w "-[FileName]),    
        setup_call_catcher_cleanup(assert_facts(ModuleName, Facts), 
            catch(ModuleName:scasp(Goal, [model(_M), tree(_T)]), Error, ( print_message(error, Error), fail ) ), 
            _Result, 
        retract_facts(ModuleName, Facts))
    ;   %print_message(error, "no scasp"-[]),
        setup_call_catcher_cleanup(assert_facts(SwishModule, Facts), 
            Command, 
            %call(Command), 
            %catch_with_backtrace(Command, Error, print_message(error, Error)), 
            %catch((true, Command), Error, ( print_message(error, Error), fail ) ), 
            _Result, 
            retract_facts(SwishModule, Facts)) 
    ),  
    %retractall(SwishModule:just_saved_scasp(_, _)), 
    show_answer(Goal). 

% answer/3
% answer(+English, with(+Scenario), -Result)
answer(English, Arg, EnglishAnswer) :- %trace, 
    le_input:parsed, 
    prepare_query(English, Arg, SwishModule, Goal, Facts, Command), 
    % this_capsule(SwishModule), 
    % translate_command(SwishModule, English, _, Goal, PreScenario), % later -->, Kbs),
    % %copy_term(Goal, CopyOfGoal), 
    % %translate_goal_into_LE(CopyOfGoal, RawGoal), name_as_atom(RawGoal, EnglishQuestion), 
    % ((Arg = with(ScenarioName), PreScenario=noscenario) -> Scenario=ScenarioName; Scenario=PreScenario), 
    % extract_goal_command(Goal, SwishModule, _InnerGoal, Command),
    % (Scenario==noscenario -> Facts = [] ; SwishModule:example(Scenario, [scenario(Facts, _)])), 
    %module(SwishModule), 
    %print_message(informational, "Calling ~w with ~w on ~w "-[Command, Facts, SwishModule]), 
    setup_call_catcher_cleanup(assert_facts(SwishModule, Facts), 
            catch_with_backtrace(Command, Error, print_message(error, Error)), 
            %catch(Command, Error, ( print_message(error, Error), fail ) ), 
            _Result, 
            retract_facts(SwishModule, Facts)),
    %print_message(informational, "The Answer is: ~w and the Result ~w"-[Command, Result]), 
    translate_goal_into_LE(Goal, RawAnswer), name_as_atom(RawAnswer, EnglishAnswer). 
    %reasoner:query_once_with_facts(Goal,Scenario,_,_E,Result).

% answer/4
% answer(+English, with(+Scenario), -Explanations, -Result) :-
% answer(at(English, Module), Arg, E, Result) :- %trace,
answer(English, Arg, E, Result) :- %trace, 
    le_input:parsed, %myDeclaredModule(Module), 
    this_capsule(SwishModule), 
    translate_command(SwishModule, English, _, Goal, PreScenario), 
    ((Arg = with(ScenarioName), PreScenario=noscenario) -> Scenario=ScenarioName; Scenario=PreScenario), 
    extract_goal_command(Goal, SwishModule, InnerGoal, _Command),
    (Scenario==noscenario -> Facts = [] ; SwishModule:example(Scenario, [scenario(Facts, _)])), !, 
    setup_call_catcher_cleanup(assert_facts(SwishModule, Facts), 
            catch((true, reasoner:query(at(InnerGoal, SwishModule),_,E,Result)), Error, ( print_message(error, Error), fail ) ), 
            _Result, 
            retract_facts(SwishModule, Facts)). 

% prepare_query/6
% prepare_query(+English, +Arguments, -Module, -Goal, -Facts, -Command)
% prepare_query(English, Arg, SwishModule, Goal, Facts, Command) :- %trace, 
%     %restore_dicts, 
%     pengine_self(SwishModule), 
%     (translate_command(SwishModule, English, GoalName, Goal, PreScenario) -> true 
%     ; ( print_message(error, "Don't understand this question: ~w "-[English]), !, fail ) ), % later -->, Kbs),
%     copy_term(Goal, CopyOfGoal),  
%     translate_goal_into_LE(CopyOfGoal, RawGoal), name_as_atom(RawGoal, EnglishQuestion), 
%     ((Arg = with(ScenarioName), PreScenario=noscenario) -> Scenario=ScenarioName; Scenario=PreScenario),
%     show_question(GoalName, Scenario, EnglishQuestion), 
%     %print_message(informational, "Scenario: ~w"-[Scenario]),
%     (Scenario==noscenario -> Facts = [] ; 
%         (SwishModule:example(Scenario, [scenario(Facts, _)]) -> 
%             true;  print_message(error, "Scenario: ~w does not exist"-[Scenario]))),
%     %print_message(informational, "Facts: ~w"-[Facts]), 
%     extract_goal_command(Goal, SwishModule, _InnerGoal, Command), !.  
%     %print_message(informational, "Command: ~w"-[Command]).

% prepare_query(+English, +Arguments, -Module, -Goal, -Facts, -Command)
prepare_query(English, Arg, SwishModule, Goal, Facts, Command) :- %trace, 
    %restore_dicts, 
    var(SwishModule), this_capsule(SwishModule), !, 
    %print_message(informational, "Module at prepare query ~w"-[SwishModule]), 
    translate_command(SwishModule, English, GoalName, Goal, PreScenario),
    %print_message(informational, "SwisModule: ~w, English ~w, GoalName ~w, Goal ~w, Scenario ~w"-[SwishModule, English, GoalName, Goal, PreScenario]),
    copy_term(Goal, CopyOfGoal),  
    translate_goal_into_LE(CopyOfGoal, RawGoal), name_as_atom(RawGoal, EnglishQuestion), 
    ((Arg = with(ScenarioName), PreScenario=noscenario) -> Scenario=ScenarioName; Scenario=PreScenario),
    show_question(GoalName, Scenario, EnglishQuestion), 
    %print_message(informational, "Scenario: ~w"-[Scenario]),
    (Scenario==noscenario -> Facts = [] ; 
        (SwishModule:example(Scenario, [scenario(Facts, _)]) -> 
            true;  print_message(error, "Scenario: ~w does not exist"-[Scenario]))),
    %print_message(informational, "Facts: ~w"-[Facts]), 
    extract_goal_command(Goal, SwishModule, _InnerGoal, Command), !.   
    %print_message(informational, "Command: ~w"-[Command]). 

% prepare_query(+English, +Arguments, +Module, -Goal, -Facts, -Command)
prepare_query(English, Arg, SwishModule, Goal, Facts, Command) :- %trace, 
    %restore_dicts, 
    nonvar(SwishModule),
    %with_output_to(string(Report), listing(dict/3)),
    %print_message(informational, "prepare_query (1): Dictionaries in memory ~w\n"-[Report]),  
    translate_command(SwishModule, English, GoalName, Goal, PreScenario),
    copy_term(Goal, CopyOfGoal),
    %print_message(informational, "prepare_query (2): translated ~w into goalname ~w goal ~w with scenario ~w\n "-[English,GoalName,Goal,PreScenario]), 
    translate_goal_into_LE(CopyOfGoal, RawGoal), name_as_atom(RawGoal, EnglishQuestion),
    ((Arg = with(ScenarioName), PreScenario=noscenario) -> Scenario=ScenarioName; Scenario=PreScenario),
    show_question(GoalName, Scenario, EnglishQuestion),  
    %print_message(informational, "prepare_query (3): Scenario: ~w"-[Scenario]), 
    (Scenario==noscenario -> Facts = [] ; 
        (SwishModule:example(Scenario, [scenario(Facts, _)]) -> 
            true;  print_message(error, "Scenario: ~w does not exist"-[Scenario]))), 
    %print_message(informational, "prepare_query (4): Facts: ~w Goal: ~w Module: ~w\n "-[Facts, Goal, SwishModule]),  
    extract_goal_command(Goal, SwishModule, _InnerGoal, Command), !.
    %print_message(informational, "prepare_query (5): Ready from ~w the command ~w\n"-[English, Command]).  

prepare_query(English, _, _, _, _, _) :- 
    print_message(error, "Don't understand this question: ~w "-[English]). 

show_question(GoalName, Scenario, NLQuestion) :- (this_capsule(M); current_module(M)),   
    (M:source_lang(en) -> print_message(informational, "Query ~w with ~w: ~w"-[GoalName, Scenario, NLQuestion]); true),
    (M:source_lang(fr) -> print_message(informational, "La question ~w avec ~w: ~w"-[GoalName, Scenario, NLQuestion]); true),
    (M:source_lang(it) -> print_message(informational, "Domanda ~w con ~w: ~w"-[GoalName, Scenario, NLQuestion]); true), 
    (M:source_lang(es) -> print_message(informational, "La pregunta ~w con ~w: ~w"-[GoalName, Scenario, NLQuestion]); true),  
    (\+(M:source_lang(_)) -> print_message(informational, "Query ~w with ~w: ~w"-[GoalName, Scenario, NLQuestion]); true),  
    !.  

show_answer(Goal) :- %trace, 
    this_capsule(M), 
    translate_goal_into_LE(Goal, RawAnswer), name_as_atom(RawAnswer, NLAnswer), 
    (M:source_lang(en) -> print_message(informational, "Answer: ~w"-[NLAnswer]); true), 
    (M:source_lang(fr) -> print_message(informational, "La réponse: ~w"-[NLAnswer]); true), 
    (M:source_lang(it) -> print_message(informational, "Risposta: ~w"-[NLAnswer]); true), 
    (M:source_lang(es) -> print_message(informational, "La respuesta: ~w"-[NLAnswer]); true),
    (\+(M:source_lang(_)) -> print_message(informational, "Answer: ~w"-[NLAnswer]); true),  % english as default
    !. 

% translate_goal_into_LE/2
% translate_goal_into_LE(+Goals_after_being_queried, -Goals_translated_into_LEnglish_as_answers)
translate_goal_into_LE((G,R), WholeAnswer) :- 
    translate_goal_into_LE(G, Answer), 
    translate_goal_into_LE(R, RestAnswers), !, 
    append(Answer, ['\n','\t',and|RestAnswers], WholeAnswer).
translate_goal_into_LE(aggregate_all(sum(V),Conditions,R), [R,is,the,sum,of,each,V,such,that,'\n', '\t'|Answer]) :-
    translate_goal_into_LE(Conditions, Answer), !.
translate_goal_into_LE(not(G), [it,is,not,the,case,that,'\n', '\t'|Answer]) :- 
    translate_goal_into_LE(G, Answer), !.
translate_goal_into_LE(Goal, ProcessedWordsAnswers) :- 
    %print_message(informational, "translated_goal_into_LE: (meta) from  ~w\n"-[Goal]), 
    Goal =.. [Pred|GoalElements], meta_dictionary([Pred|GoalElements], Types, WordsAnswer),
    process_types_or_names(WordsAnswer, GoalElements, Types, ProcessedWordsAnswers), !.
    %print_message(informational, "translated_goal_into_LE: from  ~w to ~w "-[Goal, ProcessedWordsAnswers]). 
translate_goal_into_LE(Goal, ProcessedWordsAnswers) :- 
    %print_message(informational, "translated_goal_into_LE: from  ~w\n"-[Goal]),  
    Goal =.. [Pred|GoalElements], dictionary([Pred|GoalElements], Types, WordsAnswer), 
    process_types_or_names(WordsAnswer, GoalElements, Types, ProcessedWordsAnswers), !.
    %print_message(informational, "translated_goal_into_LE: from  ~w to ~w "-[Goal, ProcessedWordsAnswers]).
translate_goal_into_LE(happens(Goal,T), Answer) :-    % simple goals do not return a list, just a literal
    Goal =.. [Pred|GoalElements], dictionary([Pred|GoalElements], Types, WordsAnswer), 
    process_types_or_names(WordsAnswer, GoalElements, Types, ProcessedWordsAnswers), 
    process_time_term(T, TimeExplain), !, 
    Answer = ['At', TimeExplain, it, occurs, that|ProcessedWordsAnswers].
translate_goal_into_LE(holds(Goal,T), Answer) :- 
    Goal =.. [Pred|GoalElements], dictionary([Pred|GoalElements], Types, WordsAnswer), 
    process_types_or_names(WordsAnswer, GoalElements, Types, ProcessedWordsAnswers), 
    process_time_term(T, TimeExplain),
    Answer = ['At', TimeExplain, it, holds, that|ProcessedWordsAnswers], !. 

process_time_term(T,ExplainT) :- var(T), name_as_atom([a, time, T], ExplainT). % in case of vars
process_time_term(T,T) :- nonvar(T), atom(T), !. 
process_time_term(T,Time) :- nonvar(T), number(T), T>100, unparse_time(T, Time), !.  
process_time_term(T,Time) :- nonvar(T), number(T), T=<100, T=Time, !.  % hack to avoid standard time transformation
process_time_term((after(T)-Var), Explain) :- var(Var), !,
    process_time_term(T, Time), 
    name_as_atom([any, time, after, Time], Explain).
process_time_term((after(T1)-before(T2)), Explain) :- !,
    process_time_term(T1, Time1), process_time_term(T2, Time2),
    name_as_atom([any, time, after, Time1, and, before, Time2], Explain).

%process_template_for_scasp/4
%process_template_for_scasp(WordsAnswer, GoalElements, Types, +FormatElements, +ProcessedWordsAnswers)
process_template_for_scasp([], _, _, [], []) :- !.
process_template_for_scasp([Word|RestWords], Elements, Types, [' @(~p:~w) '|RestFormat], [Word, TypeName|RestPrintWords]) :- 
    var(Word), matches_type(Word, Elements, Types, Type), 
    process_template_for_scasp(RestWords,  Elements, Types, RestFormat, RestPrintWords),
    tokenize_atom(Type, NameWords), delete_underscore(NameWords, [TypeName]), escape_uppercased(TypeName, _), !.
process_template_for_scasp([Word|RestWords], Elements, Types, [' @(~p:~p) '|RestFormat], [Word, TypeName|RestPrintWords]) :- 
    var(Word), matches_type(Word, Elements, Types, Type), !, 
    process_template_for_scasp(RestWords,  Elements, Types, RestFormat, RestPrintWords),
    tokenize_atom(Type, NameWords), delete_underscore(NameWords, [TypeName]).
process_template_for_scasp([Word|RestWords],  Elements, Types, RestFormat, RestPrintWords ) :- % skipping apostrofes by now
    nonvar(Word), Word = '\'', !, 
    process_template_for_scasp(RestWords,  Elements, Types, RestFormat, RestPrintWords).
process_template_for_scasp([Word|RestWords],  Elements, Types, ['~p'|RestFormat], [Word|RestPrintWords] ) :-
    op_stop(List), member(Word,List), !, 
    process_template_for_scasp(RestWords,  Elements, Types, RestFormat, RestPrintWords).
process_template_for_scasp([Word|RestWords],  Elements, Types, [' ~w '|RestFormat], [Word|RestPrintWords] ) :-
    escape_uppercased(Word, _), !, 
    %name(Word, List), 
    %print_message(informational, "processing word ~p ~q"-[Word, List]), 
    process_template_for_scasp(RestWords,  Elements, Types, RestFormat, RestPrintWords).
process_template_for_scasp([Word|RestWords],  Elements, Types, [' ~p '|RestFormat], [Word|RestPrintWords] ) :-
    process_template_for_scasp(RestWords,  Elements, Types, RestFormat, RestPrintWords).

escape_uppercased(Word, EscapedWord) :-
    name(Word, [First|Rest]), First >= 65, First =< 90,
    append([92, First|Rest], [92], NewCodes),
    name(EscapedWord, NewCodes).

assert_facts(_, []) :- !. 
assert_facts(SwishModule, [F|R]) :- nonvar(F), % print_message(informational, "asserting: ~w"-[SwishModule:F]),
    assertz(SwishModule:F), assert_facts(SwishModule, R).

retract_facts(_, []) :- !. 
retract_facts(SwishModule, [F|R]) :- nonvar(F),  %print_message(informational, "retracting: ~w"-[SwishModule:F]),
    retract(SwishModule:F), retract_facts(SwishModule, R). 

% translate_command/1
translate_command(SwishModule, English_String, GoalName, Goals, Scenario) :- %trace, 
    tokenize(English_String, Tokens, [cased(true), spaces(true), numbers(false)]),
    unpack_tokens(Tokens, UTokens), 
    clean_comments(UTokens, CTokens),
    phrase(command_(GoalName, Scenario), CTokens), 
    %print_message(informational, "GoalName ~w SwishModule ~w"-[GoalName, SwishModule]), 
    ( SwishModule:query(GoalName, Goals) -> true; (print_message(informational, "No goal named: ~w"-[GoalName]), fail) ), !. 

translate_command(_, English_String, GoalName, Goals, Scenario) :-
    tokenize(English_String, Tokens, [cased(true), spaces(true), numbers(false)]),
    unpack_tokens(Tokens, UTokens), 
    clean_comments(UTokens, CTokens), Scenario=noscenario, GoalName=nonamed, 
    (phrase(conditions(0, [], _, Goals), CTokens) ->  true  ;
        ( once(error_notice(error, Me,_, ContextTokens)), print_message(informational, "~w ~w"-[Me,ContextTokens]), CTokens=[], fail )
    ). 

command_(Goal, Scenario) --> 
    %order_, goal_(Goal), with_, scenario_name_(Scenario). 
    goal_(Goal), with_, scenario_name_(Scenario).
command_(Goal, noscenario) --> 
    goal_(Goal).

%order_ --> [answer], spaces(_).
%order_ --> [run], spaces(_).
%order_ --> [solve], spaces(_).
%order_ --> [resolve], spaces(_).

goal_(Goal) --> query_or_empty, extract_constant([with], GoalWords), spaces(_), 
    {name_as_atom(GoalWords, Goal)}. % goal by name

query_or_empty --> query_.
query_or_empty --> []. 

with_ --> [with], spaces(_).

scenario_name_(Scenario) -->  scenario_or_empty_, extract_constant([], ScenarioWords), spaces(_), 
{name_as_atom(ScenarioWords, Scenario)}. % Scenario by name

scenario_or_empty_ --> [scenario], spaces(_). 
scenario_or_empty_ --> spaces(_). 
 
% show/1
show(prolog) :-
    %print_message(informational, "About to show prolog code"), 
    show(metarules), 
    show(rules),
    show(queries),
    show(scenarios). 

show(rules) :- % trace, 
    this_capsule(SwishModule), 
    findall((Pred :- Body), 
        (dict(PredicateElements, _, _),  PredicateElements\=[], Pred=..PredicateElements,
        clause(SwishModule:Pred, Body_), unwrapBody(Body_, Body)), Predicates),
    forall(member(Clause, [(is_(A,B) :- (nonvar(B), is(A,B)))|Predicates]), portray_clause_ind(Clause)).

% 
%(op2tokens(Pred, _, OpTokens) -> % Fixing binary predicates for scasp
%( append([X|_], [Y], GoalElements),
%  append([X|OpTokens],[Y], RevGoalElements), 
%  print_message(informational, "binary op ~w"-[Pred]) ) 
%; RevGoalElements = GoalElements 
%), 

show(metarules) :- % trace, 
    this_capsule(SwishModule), 
    findall((Pred :- Body), 
        (meta_dict(PredicateElements, _, _), PredicateElements\=[], 
         Pred=..PredicateElements, clause(SwishModule:Pred, Body_), unwrapBody(Body_, Body)), Predicates),
    forall(member(Clause, Predicates), portray_clause_ind(Clause)).

show(queries) :- % trace, 
    this_capsule(SwishModule), 
    findall((query(A,B) :- true), 
        (clause(SwishModule:query(A,B), _)), Predicates),
    forall(member(Clause, Predicates), portray_clause_ind(Clause)).

show(scenarios) :- % trace, 
    this_capsule(SwishModule), 
    findall((example(A,B) :- true), 
        (clause(SwishModule:example(A,B), _)), Predicates),
    forall(member(Clause, Predicates), portray_clause_ind(Clause)).

show(templates) :-
    findall(EnglishAnswer, 
        ( ( meta_dictionary([_|GoalElements], Types, WordsAnswer) ; 
            dictionary([_|GoalElements], Types, WordsAnswer)),
        process_types_or_names(WordsAnswer, GoalElements, Types, ProcessedWordsAnswers),
        name_as_atom(ProcessedWordsAnswers, EnglishAnswer)), Templates), 
    forall(member(T, Templates), print_message(informational, "~w"-[T])). 

show(templates_scasp) :-
    findall(Term, 
        ( ( meta_dict([Pred|GoalElements], Types, WordsAnswer) ;
            dict([Pred|GoalElements], Types, WordsAnswer)),
        Goal =.. [Pred|GoalElements],
        process_template_for_scasp(WordsAnswer, GoalElements, Types, FormatEl, LE),
        atomic_list_concat(['#pred ~w ::\''|FormatEl], Format),
        Elements = [Goal|LE],
        numbervars(Elements, 1, _),
        format(atom(Term), Format, Elements)), Templates),
    forall(member(T, Templates), (atom_string(T, R), print_message(informational, "~w\'."-[R]))).

show(types) :-
    %findall(EnglishAnswer, 
    %    ( dictionary([_|GoalElements], Types, _), 
    %      member((Name-Type), Types), 
    %    process_types_or_names([Type], GoalElements, Types, ProcessedWordsAnswers),
    %    name_as_atom(ProcessedWordsAnswers, EnglishAnswer)), Templates), 
    print_message(information, "Pre-defined Types:"-[]),
    setof(Tpy, pre_is_type(Tpy), PreSet), 
    forall(member(Tp, PreSet),print_message(informational, '~a'-[Tp])), 
    print_message(informational, "Types defined in the current document:"-[]), 
    setof(Ty, is_type(Ty), Set), 
    forall(member(T, Set), print_message(informational, '~a'-[T])). 

show(scasp) :-
    show(templates_scasp), 
    show(metarules), 
    show(rules). 

show(scasp, with(Q, S)) :-
    show(scasp), 
    this_capsule(SwishModule), 
    clause(SwishModule:query(Q,Query), _),
    clause(SwishModule:example(S, [scenario(Scenario, _)]), _),
    %print_message(informational, "% scenario ~w ."-[List]),
    forall(member(Clause, Scenario), portray_clause_ind(Clause)),
    print_message(informational, "/** <examples>\n?- ? ~w .\n **/ "-[Query]).

show(scasp, with(Q)) :-
    show(scasp), 
    this_capsule(SwishModule), 
    clause(SwishModule:query(Q,Query), _),
    print_message(informational, "/** <examples>\n?- ? ~w .\n **/ "-[Query]).

unwrapBody(targetBody(Body, _, _, _, _, _), Body). 

% hack to bring in the reasoner for explanations.  
targetBody(G, false, _, '', [], _) :-
    this_capsule(SwishModule), extract_goal_command(G, SwishModule, _InnerG, Command), 
    %print_message(informational, "Reducing ~w to ~w"-[G,Command]),
    call(Command). 

dump(templates, String) :-
    findall(local_dict(Prolog, NamesTypes, Templates), (le_input:dict(Prolog, NamesTypes, Templates)), PredicatesDict),
    with_output_to(string(String01), forall(member(Clause1, PredicatesDict), portray_clause_ind(Clause1))),
    (PredicatesDict==[] -> string_concat("local_dict([],[],[]).\n", String01, String1); String1 = String01), 
    findall(local_meta_dict(Prolog, NamesTypes, Templates), (le_input:meta_dict(Prolog, NamesTypes, Templates)), PredicatesMeta),
    with_output_to(string(String02), forall(member(Clause2, PredicatesMeta), portray_clause_ind(Clause2))),
    (PredicatesMeta==[] -> string_concat("local_meta_dict([],[],[]).\n", String02, String2); String2 = String02), 
    string_concat(String1, String2, String). 

dump(templates_scasp, String) :-
    findall(Pred/N, ( ( meta_dict([Pred|GoalElements], Types, WordsAnswer) ;
                   dict([Pred|GoalElements], _, _) ),
                   length(GoalElements, N) ), 
        Functors),
    (Functors\=[] -> 
        write_functors_to_string(Functors, "", StringFunctors), 
        string_concat(":- dynamic ", StringFunctors, String0 ),
        string_concat(String0, ".\n", String1)
    ;   String1 = ""
    ), 
    findall(Term, 
        ( ( meta_dict([Pred|GoalElements], Types, WordsAnswer) ;
            dict([Pred|GoalElements], Types, WordsAnswer)),
        Goal =.. [Pred|GoalElements],
        process_template_for_scasp(WordsAnswer, GoalElements, Types, FormatEl, LE),
        atomic_list_concat(['#pred ~p :: \''|FormatEl], Format),
        Elements = [Goal|LE], 
        numbervars(Elements, 1, _), 
        format(atom(Term), Format, Elements) ), Templates),
    with_output_to(string(String2), forall(member(T, Templates), (atom_string(T, R),write(R),write("\'.\n")))),
    string_concat(String1, String2, String). 

dump(source_lang, String) :-
    le_input:source_lang(L) -> 
    with_output_to(string(String), portray_clause_ind(source_lang(L))) ; String="". 

dump(source_lang_scasp, String) :-
    le_input:source_lang(L) -> 
    with_output_to(string(String), portray_clause_ind(:- set_prolog_flag(scasp_lang, L))) ; String="".     

% #abducible
dump(abducibles_scasp, List, String) :-
    findall(Term, ( member( abducible(Abducible, _), List), Abducible\=true, format(string(Term), "#abducible ~p", [Abducible]) ), Abds), 
    with_output_to(string(String), forall(member(S, Abds), (term_string(T, S), portray_clause_ind(T)))).


dump(scasp_scenarios_queries, List, String) :- 
    findall( example(Name, Scenario), 
        (member( example(Name, Scenario), List)), Scenarios),
    %print_message(informational, "Scenarios ~w"-[Scenarios]),
    % example(one, [scenario(
    %                       [(the_service_is_delivered_before(1654423200.0):-true),  
    %                       (the_service_recipient_maintains_all_communication_within_the_confines_of(domain):-true),  
    %                       (the_service_recipient_delivers_requested_information_before(1654077600.0):-true),  
    %                       (is_signed_by_the_service_provider(the_contract):-true),  
    %                       (is_also_signed_by_the_service_recipient(the_contract):-true)
    %                       ], true)]).
    with_output_to(string(StringScenarios),
        ( forall(member(example(S, [scenario(Scenario, _)]), Scenarios),
                ( write("/* Scenario "), write(S), write("\n"), % simple comment not for PlDoc
                  forall((member(Clause, Scenario),Clause\=(abducible(_,_) :- _)), portray_clause_ind(Clause)),
                  forall((member(Clause, Scenario),Clause=(abducible(Abd,_) :- _)), 
                        (format(string(String), "#abducible ~p", [Abd]), term_string(Term, String), portray_clause_ind(Term))),
                  write("% */ \n")
                )
            )
        )
    ),
    with_output_to(string(String00), write("/** <examples>\n")), 
    findall( Query, (member( query(_, Query), List), Query\=true), Queries),
    with_output_to(string(String01), forall(member(Q, Queries), ( write("?- ? "), writeq(Q), write(".\n") ))),
    with_output_to(string(String0N), write("**/")),
    string_concat(String00, String01, String02),
    string_concat(String02, String0N, StringQueries),
    string_concat(StringScenarios, StringQueries, String). 

dump(rules, List, String) :- %trace, 
    findall((Pred :- Body), 
        (member( (Pred :- Body_), List), unwrapBody(Body_, Body)), Predicates),
    with_output_to(string(String), forall(member(Clause, Predicates), portray_clause_ind(Clause))).

dump(queries, List, String) :- 
    findall( query(Name, Query), 
        (member( query(Name, Query), List)), Predicates),
    with_output_to(string(String), forall(member(Clause, Predicates), portray_clause_ind(Clause))).

dump(scenarios, List, String) :- 
    findall( example(Name, Scenario), 
        (member( example(Name, Scenario), List)), Predicates),
    with_output_to(string(String), forall(member(Clause, Predicates), portray_clause_ind(Clause))).

dump(all, Module, List, String) :-
    %print_message(informational, " To dump all"),
	dump(templates, StringTemplates), 
    %print_message(informational, " Templates ~w"-[StringTemplates]),
	dump(rules, List, StringRules),
    dump(scenarios, List, StringScenarios),
    dump(queries, List, StringQueries), 
    string_concat(":-module(\'", Module, Module01),
    string_concat(Module01, "\', []).\n", TopHeadString),  
    dump(source_lang, SourceLang), 
    string_concat(TopHeadString, SourceLang, TopMost), 
	string_concat(TopMost, StringTemplates, HeadString),  
    string_concat(HeadString, "prolog_le(verified).\n", String0), % it has to be here to set the context
	string_concat(String0, StringRules, String1),
    string_concat(String1, StringScenarios, String2),
    string_concat(String2, StringQueries, String).   

dump_scasp(Module, List, String) :-
	dump(templates_scasp, StringTemplates), 
	dump(rules, List, StringRules),
    dump(scasp_scenarios_queries, List, StringQueriesScenarios),
    dump(abducibles_scasp, List, StringAbds),  
    string_concat(":-module(\'", Module, Module01),
    string_concat(Module01, "\', []).\n", TopHeadString), 
    dump(source_lang_scasp, SourceLang), 
    string_concat(TopHeadString, SourceLang, TopMost), 
    % headers for scasp
    string_concat("% s(CASP) Programming \n:- use_module(library(scasp)).\n% Uncomment to suppress warnings\n:- style_check(-discontiguous).\n",
                ":- style_check(-singleton).\n:- set_prolog_flag(scasp_forall, prev).\n", SCAPSHeader),
    string_concat(TopMost, SCAPSHeader, Header), 
	string_concat(Header, StringTemplates, HeadString), 
    string_concat(HeadString, StringAbds, String0), 
    %string_concat(String1, "prolog_le(verified).\n", String2), % not need for scasp
	string_concat(String0, StringRules, String1),
    string_concat(String1, StringQueriesScenarios, String). 

restore_dicts :- %trace, 
    %print_message(informational, "dictionaries being restored"),
    restore_dicts(DictEntries), 
    order_templates(DictEntries, OrderedEntries), 
    process_types_dict(OrderedEntries, Types), 
    append(OrderedEntries, Types, MRules), 
    assertall(MRules), !. % asserting contextual information

restore_dicts(DictEntries) :- %trace, 
    %myDeclaredModule(SwishModule),
    this_capsule(SwishModule), 
    %SwishModule=user,
    %print_message(informational, "the dictionaries are being restored into module ~w"-[SwishModule]),
    (SwishModule:local_dict(_,_,_) -> findall(dict(A,B,C), SwishModule:local_dict(A,B,C), ListDict) ; ListDict = []),
    (SwishModule:local_meta_dict(_,_,_) -> findall(meta_dict(A,B,C), SwishModule:local_meta_dict(A,B,C), ListMetaDict); ListMetaDict = []),
    %(local_dict(_,_,_) -> findall(dict(A,B,C), local_dict(A,B,C), ListDict) ; ListDict = []),
    %(local_meta_dict(_,_,_) -> findall(meta_dict(A,B,C), local_meta_dict(A,B,C), ListMetaDict); ListMetaDict = []),
    append(ListDict, ListMetaDict, DictEntries), 
    %print_message(informational, "the dictionaries being restored are ~w"-[DictEntries]),
    collect_all_preds(SwishModule, DictEntries, Preds),
    %print_message(informational, "the dictionaries being set dynamics are ~w"-[Preds]),
    declare_preds_as_dynamic(SwishModule, Preds). 

% collect_all_preds/3
collect_all_preds(_, DictEntries, ListPreds) :-
    findall(AA, ((member(dict(A,_,_), DictEntries); member(meta_dict(A,_,_), DictEntries)), A\=[], AA =.. A, not(predicate_property(AA,built_in))), ListPreds). 

declare_preds_as_dynamic(_, []) :- !. 
declare_preds_as_dynamic(M, [F|R]) :- functor(F, P, A),  % facts are the templates now
        dynamic([M:P/A], [thread(local), discontiguous(true)]), declare_preds_as_dynamic(M, R). 

%split_module_name(user, temporal, '') :- !. 

split_module_name(Name, Name, '') :-
   \+ sub_atom(Name, _, _, _, '+'),
   \+ sub_atom(Name, _, _, _, 'http'), !. 

split_module_name(Name, File, URL):-
	sub_atom(Name,U,1,_,'+'),
	sub_atom(Name,0,U,_,File),
	UU is U+1, 
	sub_atom(Name,UU,_,0,URL), 
	!. 
	%print_message(informational, URL). 

split_module_name(Name, Name, Name) :- % dangerous. But it maybe needed for earlier taxlog examples. 
	sub_atom(Name, _, _, _, 'http'), !. 

write_functors_to_string([F/N], Previous, StringFunctors) :- !, 
    with_output_to(string(StringF), format("~p/~d", [F,N])),
    string_concat(Previous, StringF, StringFunctors). 
write_functors_to_string([F/N|R], Previous, StringFunctors) :- !, 
    write_functors_to_string(R, Previous, NextString), 
    with_output_to(string(StringF), format("~p/~d, ", [F,N])),
    string_concat(StringF, NextString, StringFunctors). 

%%% ------------------------------------------------ Swish Interface to logical english
%% based on logicalcontracts' lc_server.pl

:- multifile prolog_colour:term_colours/2.
prolog_colour:term_colours(en(_Text),lps_delimiter-[classify]). % let 'en' stand out with other taxlog keywords
prolog_colour:term_colours(en_decl(_Text),lps_delimiter-[classify]). % let 'en_decl' stand out with other taxlog keywords


user:(answer Query with Scenario):- 
    %print_message(informational,"le_answer:answer ~w with ~w"-[Query, Scenario]), 
    answer(Query,with(Scenario)). 
user: (répondre Query avec Scenario):-
    answer(Query,with(Scenario)).
user: (risposta Query con Scenario):-
    answer(Query,with(Scenario)). 
user: (responde Query con Scenario):-
    answer(Query,with(Scenario)). 
%:- discontiguous (with)/2.
%user:(Query with Scenario):-  
%    answer(Query,with(Scenario)). 
%user:(Command1 and Command2) :-
%    call(Command1), call(Command2). 
user:answer( EnText) :- answer( EnText).
user:answer( EnText, Scenario) :- answer( EnText, Scenario).
user:answer( EnText, Scenario, Result) :- answer( EnText, Scenario, Result).
user:answer( EnText, Scenario, E, Result) :- answer( EnText, Scenario, E, Result).

%user:(show Something) :- 
%    le_answer:show(Something). 

user:(show(Something, With) ):- 
    le_answer:show(Something, With). 

user:is_it_illegal( EnText, Scenario) :- is_it_illegal( EnText, Scenario).

%user:query(Name, Goal) :- query(Name, Goal).

user:holds(Fluent, Time) :- holds(Fluent, Time). 

user:has_as_head_before(List, Head, Rest) :- has_as_head_before(List, Head, Rest). 

% for term_expansion
%user:le_taxlog_translate( en(Text), Terms) :- le_taxlog_translate( en(Text), Terms)..
%user:le_taxlog_translate( en(Text), File, Base, Terms) :- le_taxlog_translate( en(Text),  File, Base, Terms).

user:op_stop(StopWords) :- op_stop(StopWords). 

%user:targetBody(G, B, X, S, L, R) :- targetBody(G, B, X, S, L, R). 

user:restore_dicts :- restore_dicts.

% user:source_lang(L) :- source_lang(L). 

le_taxlog_translate( EnText, Terms) :- le_taxlog_translate( EnText, someFile, 1, Terms).

% Baseline is the line number of the start of Logical English text
le_taxlog_translate( en(Text), File, BaseLine, Terms) :-
    %print_message(informational,"en( ~w )"-[Text]), 
    le_input:text_to_logic(Text, Terms) -> true; showErrors(File,BaseLine). 
le_taxlog_translate( fr(Text), File, BaseLine, Terms) :-
    le_input:text_to_logic(Text, Terms) -> true; showErrors(File,BaseLine). 
le_taxlog_translate( it(Text), File, BaseLine, Terms) :-
    le_input:text_to_logic(Text, Terms) -> true; showErrors(File,BaseLine). 
le_taxlog_translate( es(Text), File, BaseLine, Terms) :-
    le_input:text_to_logic(Text, Terms) -> true; showErrors(File,BaseLine).
le_taxlog_translate( prolog_le(verified), _, _, prolog_le(verified)) :- %trace, % running from prolog file
    assertz(le_input:parsed), this_capsule(M),  
    assertz(M:just_saved_scasp(null, null)), 
    including -> true; restore_dicts. 

combine_list_into_string(List, String) :-
    combine_list_into_string(List, "", String).

combine_list_into_string([], String, String).
combine_list_into_string([HS|RestS], Previous, OutS) :-
    string_concat(Previous, HS, NewS),
    combine_list_into_string(RestS, NewS, OutS).

%user:showtaxlog :- showtaxlog.
%user:is_type(T) :- is_type(T).
%user:dict(A,B,C) :- dict(A,B,C).
%user:meta_dict(A,B,C) :- meta_dict(A,B,C). 

showtaxlog:-
    % ?????????????????????????????????????????
	% psyntax:lps_swish_clause(en(Text),Body,_Vars),
	once(text_to_logic(_,Taxlog)),
    showErrors(someFile,0), 
	writeln(Taxlog),
	fail.
showtaxlog.

% le_expanded_terms/2 is being used for term expansion in user_module_for_swish
le_expanded_terms(TaxlogTerms, ExpandedTerms) :-
    %print_message(informational, " Translated ~w"-[TaxlogTerms]), 
	(TaxlogTerms\=[]-> 
        findall(PrologTerm, (
        member(TT_,TaxlogTerms), 
        (is_list(TT_)->member(TT,TT_);TT=TT_), % the LE translator generates a list of lists... and non lists
        ((member(target(prolog),TaxlogTerms);member(target(scasp),TaxlogTerms)) -> 
            semantics2prolog(TT,_,PrologTerm);taxlog2prolog(TT,_,PrologTerm))
        ), ExpandedTerms_0) 
    ; ExpandedTerms_0 = []),
    %print_message(informational, " First Expansion ~w"-[ExpandedTerms_0]), 
    % to save as a separated file
    (member(target(prolog),TaxlogTerms) -> 
        ( myDeclaredModule(Name),  % the module in the editor
        split_module_name(Name, FileName, URL), 
        atomic_list_concat([FileName,'-prolog','.pl'], NewFileName), 
        (URL\=''->atomic_list_concat([FileName,'-prolog', '+', URL], NewModule); atomic_list_concat([FileName,'-prolog'], NewModule)),
        %print_message(informational, " Processing module ~w filename ~w URL ~w"-[Name, FileName, URL]),  
        dump(all, NewModule, ExpandedTerms_0, String), 
        %print_message(informational, " To dump this ~w"-[String]),
        update_file(NewFileName, URL, String),
        ExpandedTerms_1 = [just_saved_scasp(null, null)|ExpandedTerms_0]) ; ExpandedTerms_1 = ExpandedTerms_0),
    %print_message(informational, " Terms ~w"-[ExpandedTerms_1]), 
    (member(target(scasp),TaxlogTerms) -> 
        ( myDeclaredModule(Name),  % the module in the editor
        split_module_name(Name, FileName, URL), 
        atomic_list_concat([FileName,'-scasp','.pl'], NewFileName), 
        (URL\=''->atomic_list_concat([FileName,'-scasp', '+', URL], NewModule); atomic_list_concat([FileName,'-scasp'], NewModule)), 
        %print_message(informational, "sCASP module name ~w"-[NewModule]), 
        dump_scasp(NewModule, ExpandedTerms_0, String), 
        %print_message(informational, "sCASP content to assert: ~w \n"-[String]), 
        update_file(NewFileName, NewModule, String),
        ExpandedTerms_2 = [just_saved_scasp(NewFileName, NewModule)|ExpandedTerms_1] ) ; ExpandedTerms_2 = ExpandedTerms_1),
        ExpandedTerms = ExpandedTerms_2. 

:- multifile kp_loader:myDeclaredModule_/1. 

parse_and_query(File, Document, Question, Scenario, AnswerExplanation) :-
    %print_message(informational, "parse_and_query ~w ~w ~w ~w"-[File, Document, Question, Scenario]),
    %Answer = 'respuesta + explanation'. 
	%context_module(user), % LE programs are in the user module
	%prolog_load_context(source,File), % atom_prefix(File,'pengine://'), % process only SWISH windows
	%prolog_load_context(term_position,TP), stream_position_data(line_count,TP,Line),
    le_taxlog_translate(Document, _, 1, TaxlogTerms),
    %M = user, 
    this_capsule(M), 
    %api:set_le_program_module(M),
    %M:assert(myDeclaredModule_(M)), 
    %print_message(informational, "Expanded to be asserted on ~w "-[M]), 
	non_expanded_terms(File, TaxlogTerms, ExpandedTerms),
    %print_message(informational, "Expanded to be asserted on ~w this ~w"-[M, ExpandedTerms]), 
    %forall(member(T, ExpandedTerms), (assertz(M:T), print_message(informational, "Asserted ~w"-[M:T]))),  % simulating term expansion
    %kp_loader:assert(myDeclaredModule_(user)), 
    %myDeclaredModule(M),
    forall(member(T, [(:-module(File,[]))|ExpandedTerms]), assertz(M:T)), % simulating term expansion
    answer( Question, Scenario, AnswerExplanation). 

parse_and_query_and_explanation(File, Document, Question, Scenario, Answer) :-
    %print_message(informational, "parse_and_query and explanation ~w ~w ~w ~w"-[File, Document, Question, Scenario]),
    le_taxlog_translate(Document, _, 1, TaxlogTerms),
    this_capsule(M), 
	non_expanded_terms(File, TaxlogTerms, ExpandedTerms),
    %M:assertz(myDeclaredModule_(File)), % to enable the reasoner
    %M:assertz(kp_loader:module_api_hack(M)), 
    M:assertz(myDeclaredModule_(File)),  
    forall(member(T, [(:-module(File,[]))|ExpandedTerms]), assertz(M:T)), % simulating term expansion
    %forall(member(T, [is_a_dragon(bob), is_a_dragon(alice), is_a_parent_of(alice, bob)]), assertz(M:T)), % simulating facts addition
    %kp_loader:loaded_kp(Answer).
    hack_module_for_taxlog(M), 
    %reasoner:query(at(is_happy(A), M),_,le(LE_Explanation),_), 
    %print_message(informational, " Asserted ~w"-[ExpandedTerms]), 
    %answer( Question, Scenario, Answer). 
    answer( Question, Scenario, le(LE_Explanation), _Result), 
    %with_output_to(string(Answer), write(LE_Explanation)). 
    produce_html_explanation(LE_Explanation, Answer). 

% non_expanded_terms/2 is just as the one above, but with semantics2prolog2 instead of semantics2prolog that has many other dependencies. 
non_expanded_terms(Name, TaxlogTerms, ExpandedTerms) :-
    %print_message(informational, " Translated ~w"-[TaxlogTerms]), 
	(TaxlogTerms\=[]-> 
        findall(PrologTerm, (
        member(TT_,TaxlogTerms), 
        (is_list(TT_)->member(TT,TT_);TT=TT_), % the LE translator generates a list of lists... and non lists
        ((member(target(prolog),TaxlogTerms);member(target(scasp),TaxlogTerms)) -> 
            semantics2prolog2(TT,_,PrologTerm); true) % disabling taxlog translation
        ), ExpandedTerms_0) 
    ; ExpandedTerms_0 = []),
    % member(target(prolog),TaxlogTerms), 
    %print_message(informational, " Expanded ~w"-[ExpandedTerms_0]), 
    %kp_loader:myDeclaredModule(Name),
    %print_message(informational, " Module ~w "-[Name]), 
    % ExpandedTerms=ExpandedTerms_0. 
    % to save as a separated file
    (member(target(prolog),TaxlogTerms) -> 
        ( %myDeclaredModule(Name),  % the module in the editor
        split_module_name(Name, FileName, URL), 
        atomic_list_concat([FileName,'-prolog','.pl'], NewFileName), 
        (URL\=''->atomic_list_concat([FileName,'-prolog', '+', URL], NewModule); atomic_list_concat([FileName,'-prolog'], NewModule)),
        %print_message(informational, " Processing module ~w filename ~w URL ~w"-[Name, FileName, URL]),  
        dump(all, NewModule, ExpandedTerms_0, String), 
        %print_message(informational, " To dump this ~w"-[String]),
        update_file(NewFileName, URL, String),
        ExpandedTerms_1 = [just_saved_scasp(null, null)|ExpandedTerms_0]) ; ExpandedTerms_1 = ExpandedTerms_0),
    %print_message(informational, " Terms ~w"-[ExpandedTerms_1]), 
    (member(target(scasp),TaxlogTerms) -> 
        ( %myDeclaredModule(Name),  % the module in the editor
        split_module_name(Name, FileName, URL), 
        atomic_list_concat([FileName,'-scasp','.pl'], NewFileName), 
        (URL\=''->atomic_list_concat([FileName,'-scasp', '+', URL], NewModule); atomic_list_concat([FileName,'-scasp'], NewModule)), 
        %print_message(informational, "sCASP module name ~w"-[NewModule]), 
        dump_scasp(NewModule, ExpandedTerms_0, String), 
        %print_message(informational, "sCASP content to assert: ~w \n"-[String]), 
        update_file(NewFileName, NewModule, String),
        ExpandedTerms_2 = [just_saved_scasp(NewFileName, NewModule)|ExpandedTerms_1] ) ; ExpandedTerms_2 = ExpandedTerms_1),
        ExpandedTerms = ExpandedTerms_2. 

clean_explanation([], []) :- !. 
clean_explanation([s(P,_Ref, _Source, _, _, R)|RestConj], [s(P, RR)|NewConj]) :- 
    clean_explanation(R, RR), clean_explanation(RestConj, NewConj). 
clean_explanation([f(P,_Ref, _Source, _, _, R)|RestConj], [f(P, RR)|NewConj]) :- 
    clean_explanation(R, RR), clean_explanation(RestConj, NewConj).

produce_html_explanation(le_Explanation(Trees), Explanation) :-
    explanationLEHTML(Trees,HTML), 
    % phrase(html( 
	% 	div([ 'data-render'('An explanation')],[
    %         div([], ul(id="myUL", HTML))
	% 	]) 
	% ), ExplanationInHtml),
    phrase(html(HTML), ExplanationInHtml),
    with_output_to(string(Explanation), print_html(ExplanationInHtml)). 

explanationLEHTML(s(G,_Ref,_,_,_,C),[li(title="Rule inference",[span(class=Class," "), b(G)|RestTree])]) :- 
    %Navigator=' a rule', 
    explanationLEHTML(C,CH), 
    (CH\=[] -> 
        ( RestTree =  ul(class="nested", ['because'|CH]), Class = "box" )
    ;   ( RestTree = [], Class = "leaf" )
    ). 
explanationLEHTML(u(G,_Ref,_,_,_,[]),[li(title="Unknown",["~w ?"-[G],Navigator])]) :- Navigator=' a hypothesis'. 
explanationLEHTML(f(G,_Ref,_,_,_,C),[li(title="Failed goal",[span(class=Class, " "), span(style="color:red","It cannot be proved that "), b(G)|RestTree])]) :- 
    %Navigator=' in the rules', 
    explanationLEHTML(C,CH), 
    %print_message(informational, "G vs C: ~w .. ~w ... ~w"-[G, C, CH]), 
    %(CH\=[] -> (C=[s(_,_,_,_,_,[])] -> Because = 'although' ;  Because = 'because'); Because=''). % this is filtered out before (reasoner.pl)
    (CH\=[] -> 
        ( RestTree =  ul(class="nested", ['because'|CH]), Class = "box" )
    ;   ( RestTree = [], Class = "leaf" )
    ).
explanationLEHTML([C1|Cn],CH) :- explanationLEHTML(C1,CH1), explanationLEHTML(Cn,CHn), (CHn\=[] -> Joint = ['and '|CHn]; Joint = CHn), append(CH1,Joint,CH).
    %append(CH1,CHn,CH).
explanationLEHTML([],[]).

%sandbox:safe_meta(term_singletons(X,Y), [X,Y]).

sandbox:safe_primitive(le_answer:answer( _EnText)).
sandbox:safe_primitive(le_answer:show( _Something)).
sandbox:safe_primitive(le_answer:show( _Something, _With)).
sandbox:safe_primitive(le_answer:answer( _EnText, _Scenario)).
sandbox:safe_primitive(le_answer:answer( _EnText, _Scenario, _Result)).
sandbox:safe_primitive(le_answer:answer( _EnText, _Scenario, _Explanation, _Result)).
sandbox:safe_primitive(le_answer:le_taxlog_translate( _EnText, _File, _Baseline, _Terms)).
sandbox:safe_primitive(le_answer:translate_goal_into_LE(_,_)). 
sandbox:safe_primitive(le_answer:dump_scasp(_,_,_)). 
sandbox:safe_primitive(current_output(_)). 
sandbox:safe_primitive(le_answer:(show _)). 
sandbox:safe_primitive(le_answer:parse_and_query(_,_,_,_,_)).
sandbox:safe_primitive(le_answer:parse_and_query_and_explanation(_,_,_,_,_)). 
sandbox:safe_primitive(kp_loader:module_api_hack(_)).

%sandbox:safe_primitive(term_singletons(_,_)).  % this would not work as term_singletons/2 is an undefined, C-based primitive