/* le_input: a prolog module with predicates to translate from an 
extended version of Logical English into the Prolog or Taxlogtemplate_decl

   http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

Main predicate: text_to_logic(String to be translated, Translation)

Main DCG nonterminal: document(Translation)

See at the end the predicate le_taxlog_translate to be used from SWISH

It assumes an entry with the following structure. One of these expressions:

the meta predicates are:
the predicates are:
the templates are:
the timeless predicates are:
the event predicates are:
the fluents are:
the time-varying predicates are:

followed by the declarations of all the corresponding predicates mentioned in the 
knowledge base. 

Each declarations define a template with the variables and other words required to
describe a relevant relation. It is a comma separated list of templates which ends
with a period. 

After that period, one of the following statement introduces the knowledge base:

the knowledge base includes: 
the knowledge base <Name> includes: 

And it is followed by the rules and facts written in Logical English syntax. 
Each rule must end with a period. 

Indentation is used to organize the and/or list of conditions by strict
observance of one condition per line with a level of indentation that 
corresponds to each operator and corresponding conditions. 

Similarly, there may be sections for scenarios and queries, like:

--
scenario test2 is:
   borrower pays an amount to lender on 2015-06-01T00:00:00. 
--

and

--
query one is:
for which event:
 the small business restructure rollover applies to the event.

query two is:
 which tax payer is a party of which event.

query three is:
 A first time is after a second time
 and the second time is immediately before the first time.
--

 which can then be used on the new command interface of LE on SWISH as defined in module le_answer.pl
(e.g. answer/1 and others querying predicates):

? answer("query one with scenario test"). 

*/

:- module(le_input, 
    [document/3, text_to_logic/2,
    predicate_decl/4, showErrors/2,
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
    dictionary/3, meta_dictionary/3, dict/3, meta_dict/3,
    parsed/0, source_lang/1, including/0, just_saved_scasp/2,
    this_capsule/1, unpack_tokens/2, clean_comments/2,
    query_/2, extract_constant/4, spaces/3, name_as_atom/2, process_types_or_names/4,
    matches_name/4, matches_type/4, delete_underscore/2, add_determiner/2, proper_det/2,
    portray_clause_ind/1, order_templates/2, process_types_dict/2,
    assertall/1,asserted/1, 
    update_file/3, myDeclaredModule/1, conditions/6
    ]).

:- multifile sandbox:safe_primitive/1.

:- use_module('./tokenize/prolog/tokenize.pl').

:- if(current_module(swish)). 
:- use_module('le_swish.pl'). % module to handle the gitty filesystem
:- else.
:- use_module('le_local.pl'). % module to handle the local filesystem 
:- endif.

:- use_module('reasoner.pl').
:- use_module(library(prolog_stack)).
:- thread_local text_size/1, error_notice/4, dict/3, meta_dict/3, example/2, local_dict/3, local_meta_dict/3,
                last_nl_parsed/1, kbname/1, happens/2, initiates/3, terminates/3, is_type/1, is_/2, 
                predicates/1, events/1, fluents/1, metapredicates/1, parsed/0, source_lang/1, including/0, just_saved_scasp/2. 
:- discontiguous statement/3, declaration/4, _:example/2, _:query/2, _:is_/2. 

% Main clause: text_to_logic(+String,-Clauses) is det
% Errors are added to error_notice 
% text_to_logic/2
text_to_logic(String_, Translation) :-
    % hack to ensure a newline at the end, for the sake of error reporting:
    ((sub_atom(String_,_,1,0,NL), memberchk(NL,['\n','\r']) ) -> String=String_ ; atom_concat(String_,'\n',String)),
    tokenize(String, Tokens, [cased(true), spaces(true), numbers(true)]),
    retractall(last_nl_parsed(_)), asserta(last_nl_parsed(1)), % preparing line counting
    unpack_tokens(Tokens, UTokens), 
    clean_comments(UTokens, CTokens), !, 
    %print_message(informational, "Tokens: ~w"-[CTokens]), 
    phrase(document(Translation), CTokens).
    %print_message(informational, "Translation: ~w"-[Translation]). 
    %with_output_to(string(Report), listing(dict/3)),
    %print_message(informational, "Dictionaries in memory after loading and parsing ~w\n"-[Report]). 
    %( phrase(document(Translation), CTokens) -> 
    %    ( print_message(informational, "Translation: ~w"-[Translation]) )
    %;   ( print_message(informational, "Translation failed: ~w"-[CTokens]), Translation=[], fail)). 

% document/3 (or document/1 in dcg)
document(Translation, In, Rest) :- 
    (parsed -> retractall(parsed); true), 
    (including -> retract(including); true), 
    (source_lang(_L) -> retractall(source_lang(_)) ; true),
    phrase(header(Settings), In, AfterHeader), !, %print_message(informational, "Declarations completed: ~w"-[Settings]), 
    phrase(content(Content), AfterHeader, Rest), 
    append(Settings, Content, Original), !,
    append(Original, [if(is_(A,B), (nonvar(B), is(A,B)))], Translation), % adding def of is_2 last!  
    assertz(parsed). 

% header parses all the declarations and assert them into memory to be invoked by the rules. 
% header/3
header(Settings, In, Next) :- 
    length(In, TextSize), % after comments were removed
    phrase(settings(DictEntries, Settings_), In, Next), 
    fix_settings(Settings_, Settings2), 
    RulesforErrors = [(text_size(TextSize))|Settings2], % is text_size being used? % asserting the Settings too! predicates, events and fluents
    included_files(Settings2, RestoredDictEntries, CollectedRules), 
    append(Settings2, CollectedRules, Settings), 
    append(DictEntries, RestoredDictEntries, AllDictEntries), 
    order_templates(AllDictEntries, OrderedEntries), 
    process_types_dict(OrderedEntries, Types), 
    %print_message(informational, "types ~w rules ~w"-[Types, CollectedRules]),
    append(OrderedEntries, RulesforErrors, SomeRules),
    append(SomeRules, Types, MRules), 
    %print_message(informational, "rules ~w"-[MRules]),
    assertall(MRules), !. % asserting contextual information
header(_, Rest, _) :- 
    asserterror('LE error in the header ', Rest), 
    fail.

fix_settings(Settings_, Settings2) :-
    ( member(target(_), Settings_) -> Settings1 = Settings_ ; Settings1 = [target(taxlog)|Settings_] ), !,  % taxlog as default
    Settings2 = [query(null, true), example(null, []), abducible(true,true)|Settings1]. % a hack to stop the loop when query is empty

included_files(Settings2, RestoredDictEntries, CollectedRules) :-
    member(in_files(ModuleNames), Settings2),   % include all those files and get additional DictEntries before ordering
    print_message(informational, "Module Names ~w\n"-[ModuleNames]),
    assertz(including), !, % cut to prevent escaping failure of load_all_files
    load_all_files(ModuleNames, RestoredDictEntries, CollectedRules), 
    print_message(informational, "Restored Entries ~w\n"-[RestoredDictEntries]). 
included_files(_, [], []). 

%load_all_files/2
%load the prolog files that correspond to the modules names listed in the section of inclusion
%and produces the list of entries that must be added to the dictionaries
load_all_files([], [], []).
load_all_files([Name|R], AllDictEntries, AllRules) :- 
    print_message(informational, "Loading ~w"-[Name]),
    split_module_name(Name, File, URL),  
    print_message(informational, "File ~w URL ~w"-[File, URL]),
    concat(File, "-prolog", Part1), concat(Part1, ".pl", Filename),  
    (URL\=''->atomic_list_concat([File,'-prolog', '+', URL], NewName); atomic_list_concat([File,'-prolog'], NewName)),
    print_message(informational, "File ~w FullName ~w"-[Filename, NewName]),
    load_file_module(Filename, NewName, true), !, 
    print_message(informational, "the dictionaries of ~w being restored into module ~w"-[NewName]),
    (NewName:local_dict(_,_,_) -> findall(dict(A,B,C), NewName:local_dict(A,B,C), ListDict) ; ListDict = []),
    (NewName:local_meta_dict(_,_,_) -> findall(meta_dict(A,B,C), NewName:local_meta_dict(A,B,C), ListMetaDict); ListMetaDict = []),
    append(ListDict, ListMetaDict, DictEntries), 
    %print_message(informational, "the dictionaries being restored are ~w"-[DictEntries]),
    %listing(NewName:_), 
    findall(if(H,B), (member(dict(E, _,_), DictEntries), E\=[], H=..E, clause(NewName:H, B)), Rules), 
    findall(Pred, (member(dict(E,_,_), ListDict), E\=[], Pred=..E), ListOfPred),
    findall(MPred, (member(dict(ME,_,_), ListMetaDict), ME\=[], MPred=..ME), ListOfMPred),
    append([predicates(ListOfPred), metapredicates(ListOfMPred)], Rules, TheseRules), % for term expansion     
    %print_message(informational, "rules to copy ~w"-[Rules]),
    %collect_all_preds(SwishModule, DictEntries, Preds),
    %print_message(informational, "the dictionaries being set dynamics are ~w"-[Preds]),
    %declare_preds_as_dynamic(SwishModule, Preds)
    %print_message(informational, "Loaded ~w"-[Filename]),
    load_all_files(R, RDict, NextRules),
    append(RDict, DictEntries, AllDictEntries),
    append(TheseRules, NextRules, AllRules). 
load_all_files([Filename|_], [], []) :-
    print_message(informational, "Failed to load file ~w"-[Filename]), fail.   

% Experimental rules for processing types:
process_types_dict(Dictionary, Type_entries) :- 
    findall(Word, 
    (   (member(dict([_|GoalElements], Types, _), Dictionary);
        member(meta_dict([_|GoalElements], Types, _), Dictionary)), 
        member((_Name-Type), Types), 
        process_types_or_names([Type], GoalElements, Types, TypeWords),
        concat_atom(TypeWords, '_', Word), Word\=''), Templates), 
    (Templates\=[] -> setof(is_type(Ty), member(Ty, Templates), Type_entries) ; Type_entries = []).


% process_types_or_names/4
process_types_or_names([], _, _, []) :- !.
process_types_or_names([Word|RestWords], Elements, Types, PrintExpression ) :- 
    atom(Word), concat_atom(WordList, '_', Word), !, 
    process_types_or_names(RestWords,  Elements, Types, RestPrintWords),
    append(WordList, RestPrintWords, PrintExpression).
process_types_or_names([Word|RestWords], Elements, Types, PrintExpression ) :- 
    var(Word), matches_name(Word, Elements, Types, Name), !, 
    process_types_or_names(RestWords,  Elements, Types, RestPrintWords),
    tokenize_atom(Name, NameWords), delete_underscore(NameWords, CNameWords),
    add_determiner(CNameWords, PrintName), append(['*'|PrintName], ['*'|RestPrintWords], PrintExpression).
process_types_or_names([Word|RestWords], Elements, Types, [PrintWord|RestPrintWords] ) :- 
    matches_type(Word, Elements, Types, date), 
    ((nonvar(Word), number(Word)) -> unparse_time(Word, PrintWord); PrintWord = Word), !, 
    process_types_or_names(RestWords,  Elements, Types, RestPrintWords). 
process_types_or_names([Word|RestWords], Elements, Types, [PrintWord|RestPrintWords] ) :- 
    matches_type(Word, Elements, Types, day), 
    ((nonvar(Word), number(Word)) -> unparse_time(Word, PrintWord); PrintWord = Word), !, 
    process_types_or_names(RestWords,  Elements, Types, RestPrintWords). 
process_types_or_names([Word|RestWords],  Elements, Types, Output) :-
    compound(Word), 
    translate_goal_into_LE(Word, PrintWord), !, % cut the alternatives
    process_types_or_names(RestWords,  Elements, Types, RestPrintWords),
    append(PrintWord, RestPrintWords, Output). 
process_types_or_names([Word|RestWords],  Elements, Types, [Word|RestPrintWords] ) :-
    process_types_or_names(RestWords,  Elements, Types, RestPrintWords).


% Experimental rules for reordering of templates
% order_templates/2
order_templates(NonOrdered, Ordered) :-
	predsort(compare_templates, NonOrdered, Ordered).

compare_templates(<, meta_dict(_,_,_), dict(_,_,_)). 

compare_templates(=, dict(_,_,T1), dict(_,_,T2)) :- T1 =@= T2. 
compare_templates(<, dict(_,_,T1), dict(_,_,T2)) :- length(T1, N1), length(T2, N2), N1>N2. 
compare_templates(<, dict(_,_,T1), dict(_,_,T2)) :- length(T1, N), length(T2, N), template_before(T1, T2).  

compare_templates(>, Dict1, Dict2) :- not(compare_templates(=, Dict1, Dict2)), not(compare_templates(<, Dict1, Dict2)). 

compare_templates(=, meta_dict(_,_,T1), meta_dict(_,_,T2)) :- T1 =@= T2. 
compare_templates(<, meta_dict(_,_,T1), meta_dict(_,_,T2)) :- length(T1, N1), length(T2, N2), N1>N2. 
compare_templates(<, meta_dict(_,_,T1), meta_dict(_,_,T2)) :- length(T1, N), length(T2, N), template_before(T1, T2).  

template_before([H1], [H2]) :- H1 =@= H2. 
template_before([H1|_R1], [H2|_R2]) :- nonvar(H1), var(H2).
template_before([H1|_R1], [H2|_R2]) :- H1 @> H2. 
template_before([H1|R1], [H2|R2]) :- H1=@=H2, template_before(R1, R2). 


/* --------------------------------------------------------- LE DCGs */
% settings/2 or /4
settings(AllR, AllS) --> 
    spaces_or_newlines(_), declaration(Rules,Setting), settings(RRules, RS), 
    {append(Setting, RS, AllS), append(Rules, RRules, AllR)}, !.
settings([], [], Stay, Stay) :- !, 
    ( phrase(rules_previous(_), Stay, _) ; 
      phrase(scenario_, Stay, _)  ;  
      phrase(query_, Stay, _) ).  
    % settings ending with the start of the knowledge base or scenarios or queries. 
settings(_, _, Rest, _) :- 
    asserterror('LE error in the declarations on or before ', Rest), 
    fail.
settings([], [], Stay, Stay).

% content structure: cuts added to avoid search loop
% content/1 or /3
content(T) --> %{print_message(informational, "going for KB:"-[])},  
    spaces_or_newlines(_), rules_previous(Kbname), %{print_message(informational, "KBName: ~w"-[Kbname])}, 
    kbase_content(S),  %{print_message(informational, "KB: ~w"-[S])}, 
    content(R), 
    {append([kbname(Kbname)|S], R, T)}, !.
content(T) --> %{print_message(informational, "going for scenario:"-[])},
    spaces_or_newlines(_), scenario_content(S), !, %{print_message(informational, "scenario: ~w"-[S])},
    content(R), 
    {append(S, R, T)}, !.
content(T) --> %{print_message(informational, "going for query:"-[])},
    spaces_or_newlines(_), query_content(S), !, content(R), 
    {append(S, R, T)}, !.
content([]) --> 
    spaces_or_newlines(_), []. 
content(_, Rest, _) :- 
    asserterror('LE error in the content ', Rest), 
    fail.

% kbase_content/1 or /3
kbase_content(T) --> 
    spaces_or_newlines(_),  statement(S),  kbase_content(R),
    {append(S, R, T)}, !. 
kbase_content([]) --> 
    spaces_or_newlines(_), [].
kbase_content(_, Rest, _) :- 
    asserterror('LE error in a knowledge base ', Rest), 
    fail.

% declaration/2 or /4
% target
declaration([], [target(Language)]) --> % one word description for the language: prolog, taxlog
    spaces(_), [the], spaces(_), [target], spaces(_), [language], spaces(_), [is], spaces(_), colon_or_not_, 
    spaces(_), [Language], spaces(_), period, !, {assertz(source_lang(en))}.
% french: la langue cible est : prolog 
declaration([], [target(Language)]) --> % one word description for the language: prolog, taxlog
    spaces(_), [la], spaces(_), [langue], spaces(_), [cible], spaces(_), [est], spaces(_), colon_or_not_, 
    spaces(_), [Language], spaces(_), period, !, {assertz(source_lang(fr))}.
% italian: il linguaggio destinazione è : prolog 
declaration([], [target(Language)]) --> % one word description for the language: prolog, taxlog
    spaces(_), [il], spaces(_), [linguaggio], spaces(_), [destinazione], spaces(_), [è], spaces(_), colon_or_not_, 
    spaces(_), [Language], spaces(_), period, !, {assertz(source_lang(it))}.
% spanish: el lenguaje objetivo es: prolog
declaration([], [target(Language)]) --> % one word description for the language: prolog, taxlog
    spaces(_), [el], spaces(_), [lenguaje], spaces(_), [objetivo], spaces(_), [es], spaces(_), colon_or_not_, 
    spaces(_), [Language], spaces(_), period, !, {assertz(source_lang(es))}.

% meta predicates
declaration(Rules, [metapredicates(MetaTemplates)]) -->
    meta_predicate_previous, list_of_meta_predicates_decl(Rules, MetaTemplates), !.
%timeless or just templates
declaration(Rules, [predicates(Templates)]) -->
    predicate_previous, list_of_predicates_decl(Rules, Templates), !.
%events
declaration(Rules, [events(EventTypes)]) -->
    event_predicate_previous, list_of_predicates_decl(Rules, EventTypes), !.
%time varying
declaration(Rules, [fluents(Fluents)]) -->
    fluent_predicate_previous, list_of_predicates_decl(Rules, Fluents), !.
%files to be included
declaration([kbname(KBName)], [in_files(Files)]) -->
    files_to_include_previous(KBName), list_of_files(Files), !.
%
declaration(_, _, Rest, _) :- 
    asserterror('LE error in a declaration on or before ', Rest), 
    fail.

colon_or_not_ --> [':'], spaces(_).
colon_or_not_ --> []. 

meta_predicate_previous --> 
    spaces(_), [the], spaces(_), [metapredicates], spaces(_), [are], spaces(_), [':'], spaces_or_newlines(_).
meta_predicate_previous --> 
    spaces(_), [the], spaces(_), [meta], spaces(_), [predicates], spaces(_), [are], spaces(_), [':'], spaces_or_newlines(_).
meta_predicate_previous --> 
    spaces(_), [the], spaces(_), [meta], spaces(_), ['-'], spaces(_), [predicates], spaces(_), [are], spaces(_), [':'], spaces_or_newlines(_).
% french : les modèles sont :
meta_predicate_previous --> 
    spaces(_), [les], spaces(_), ['méta'], spaces(_), ['modèles'], spaces(_), [sont], spaces(_), [':'], spaces_or_newlines(_).
% italian: i predicati sono:
meta_predicate_previous --> 
    spaces(_), [i], spaces(_), [meta], spaces(_), [modelli], spaces(_), [sono], spaces(_), [':'], spaces_or_newlines(_).
% spanish: los predicados son:
meta_predicate_previous --> 
    spaces(_), [los], spaces(_), [meta], spaces(_), [predicados], spaces(_), [son], spaces(_), [':'], spaces_or_newlines(_).

predicate_previous --> 
    spaces(_), [the], spaces(_), [predicates], spaces(_), [are], spaces(_), [':'], spaces_or_newlines(_).
predicate_previous --> 
    spaces(_), [the], spaces(_), [templates], spaces(_), [are], spaces(_), [':'], spaces_or_newlines(_).
predicate_previous --> 
    spaces(_), [the], spaces(_), [timeless], spaces(_), [predicates], spaces(_), [are], spaces(_), [':'], spaces_or_newlines(_).
% french : les modèles sont :
predicate_previous --> 
    spaces(_), [les], spaces(_), ['modèles'], spaces(_), [sont], spaces(_), [':'], spaces_or_newlines(_).
% italian: i predicati sono:
predicate_previous --> 
    spaces(_), [i], spaces(_), [modelli], spaces(_), [sono], spaces(_), [':'], spaces_or_newlines(_).
% spanish: los predicados son:
predicate_previous --> 
    spaces(_), [los], spaces(_), [predicados], spaces(_), [son], spaces(_), [':'], spaces_or_newlines(_).

event_predicate_previous --> 
    spaces(_), [the], spaces(_), [event], spaces(_), [predicates], spaces(_), [are], spaces(_), [':'], spaces_or_newlines(_).

fluent_predicate_previous --> 
    spaces(_), [the], spaces(_), [fluents], spaces(_), [are], spaces(_), [':'], spaces_or_newlines(_).
fluent_predicate_previous --> 
    spaces(_), [the], spaces(_), [time], ['-'], [varying], spaces(_), [predicates], spaces(_), [are], spaces(_), [':'], spaces_or_newlines(_).

files_to_include_previous(KBName) --> 
    spaces_or_newlines(_), [the], spaces(_), ['knowledge'], spaces(_), [base], extract_constant([includes], NameWords), [includes], 
    spaces(_), [these], spaces(_), [files], spaces(_), [':'], !, spaces_or_newlines(_), {name_as_atom(NameWords, KBName)}.

% at least one predicate declaration required
list_of_predicates_decl([], []) --> spaces_or_newlines(_), next_section, !. 
list_of_predicates_decl([Ru|Rin], [F|Rout]) --> spaces_or_newlines(_), predicate_decl(Ru,F), comma_or_period, list_of_predicates_decl(Rin, Rout), !.
list_of_predicates_decl(_, _, Rest, _) :- 
    asserterror('LE error found in a template declaration ', Rest), 
    fail.

% at least one predicate declaration required
list_of_meta_predicates_decl([], []) --> spaces_or_newlines(_), next_section, !. 
list_of_meta_predicates_decl([Ru|Rin], [F|Rout]) --> 
    spaces_or_newlines(_), meta_predicate_decl(Ru,F), comma_or_period, list_of_meta_predicates_decl(Rin, Rout).
list_of_meta_predicates_decl(_, _, Rest, _) :- 
    asserterror('LE error found in the declaration of a meta template ', Rest), 
    fail.

list_of_files([]) --> spaces_or_newlines(_), next_section, !.
list_of_files([Filename|Rout]) --> spaces_or_newlines(_), extract_string([Filename]), list_of_files(Rout), !.
    %{name_as_atom(NameWords, Filename)}.
list_of_files(_, Rest, _) :- 
    asserterror('LE error found in a file to include ', Rest), 
    fail.

% next_section/2
% a hack to avoid superflous searches  format(string(Mess), "~w", [StopHere]), print_message(informational, Message), 
next_section(StopHere, StopHere)  :-
    phrase(meta_predicate_previous, StopHere, _), !. % format(string(Message), "Next meta predicates", []), print_message(informational, Message).

next_section(StopHere, StopHere)  :-
    phrase(predicate_previous, StopHere, _), !. % format(string(Message), "Next predicates", []), print_message(informational, Message).

next_section(StopHere, StopHere)  :-
    phrase(event_predicate_previous, StopHere, _), !. % format(string(Message), "Next ecent predicates", []), print_message(informational, Message).

next_section(StopHere, StopHere)  :-
    phrase(fluent_predicate_previous, StopHere, _), !. % format(string(Message), "Next fluents", []), print_message(informational, Message).

next_section(StopHere, StopHere)  :-
    phrase(files_to_include_previous(_), StopHere, _), !.

next_section(StopHere, StopHere)  :-
    phrase(rules_previous(_), StopHere, _), !. % format(string(Message), "Next knowledge base", []), print_message(informational, Message).

next_section(StopHere, StopHere)  :-
    phrase(scenario_, StopHere, _), !. % format(string(Message), "Next scenario", []), print_message(informational, Message).

next_section(StopHere, StopHere)  :-
    phrase(query_, StopHere, _).  % format(string(Message), "Next query", []), print_message(informational, Message).

% predicate_decl/2
predicate_decl(dict([Predicate|Arguments],TypesAndNames, Template), Relation) -->
    spaces(_), template_decl(RawTemplate), 
    {build_template(RawTemplate, Predicate, Arguments, TypesAndNames, Template),
     Relation =.. [Predicate|Arguments]}, !.
% we are using this resource of the last clause to record the error and its details
% not very useful with loops, of course. 
% error clause
predicate_decl(_, _, Rest, _) :- 
    asserterror('LE error found in a declaration ', Rest), 
    fail.

% meta_predicate_decl/2
meta_predicate_decl(meta_dict([Predicate|Arguments],TypesAndNames, Template), Relation) -->
    spaces(_), template_decl(RawTemplate), 
    {build_template(RawTemplate, Predicate, Arguments, TypesAndNames, Template),
     Relation =.. [Predicate|Arguments]}.
meta_predicate_decl(_, _, Rest, _) :- 
    asserterror('LE error found in a meta template declaration ', Rest), 
    fail.

rules_previous(default) --> 
    spaces_or_newlines(_), [the], spaces(_), [rules], spaces(_), [are], spaces(_), [':'], spaces_or_newlines(_), !.
rules_previous(KBName) --> 
    spaces_or_newlines(_), [the], spaces(_), ['knowledge'], spaces(_), [base], extract_constant([includes], NameWords), [includes], spaces(_), [':'], !, spaces_or_newlines(_),
    {name_as_atom(NameWords, KBName)}.
rules_previous(default) -->  % backward compatibility
    spaces_or_newlines(_), [the], spaces(_), ['knowledge'], spaces(_), [base], spaces(_), [includes], spaces(_), [':'], spaces_or_newlines(_). 
% italian: la base di conoscenza <nome> include
rules_previous(KBName) --> 
    spaces_or_newlines(_), [la], spaces(_), [base], spaces(_), [di], spaces(_), [conoscenza], spaces(_), extract_constant([include], NameWords), [include], spaces(_), [':'], !, spaces_or_newlines(_),
    {name_as_atom(NameWords, KBName)}.
% french: la base de connaissances dont le nom est <nom> comprend :
rules_previous(KBName) --> 
    spaces_or_newlines(_), [la], spaces(_), [base], spaces(_), [de], spaces(_), [connaissances], spaces(_), [dont], spaces(_), [le], spaces(_), [nom], spaces(_), [est], extract_constant([comprend], NameWords), [comprend], spaces(_), [':'], !, spaces_or_newlines(_),
    {name_as_atom(NameWords, KBName)}.
% spanish: la base de conocimiento <nombre> incluye: 
rules_previous(KBName) --> 
    spaces_or_newlines(_), [la], spaces(_), [base], spaces(_), [de], spaces(_), [conocimiento], extract_constant([incluye], NameWords), [incluye], spaces(_), [':'], !, spaces_or_newlines(_),
    {name_as_atom(NameWords, KBName)}.

% scenario_content/1 or /3
% a scenario description: assuming one example -> one scenario -> one list of facts.
scenario_content(Scenario) -->
    scenario_, extract_constant([is, es, est, è], NameWords), is_colon_, newline,
    %list_of_facts(Facts), period, !, 
    spaces(_), assumptions_(Assumptions), !, % period is gone
    {name_as_atom(NameWords, Name), Scenario = [example( Name, [scenario(Assumptions, true)])]}.

scenario_content(_,  Rest, _) :- 
    asserterror('LE error found around this scenario expression: ', Rest), fail.

% query_content/1 or /3
% statement: the different types of statements in a LE text
% a query
query_content(Query) -->
    query_, extract_constant([is, es, est, è], NameWords), is_colon_, spaces_or_newlines(_),
    query_header(Ind0, Map1),  
    conditions(Ind0, Map1, _, Conds), !, period,  % period stays!
    {name_as_atom(NameWords, Name), Query = [query(Name, Conds)]}. 

query_content(_, Rest, _) :- 
    asserterror('LE error found around this expression: ', Rest), fail.

% (holds_at(_149428,_149434) if 
% (happens_at(_150138,_150144),
%           initiates_at(_150138,_149428,_150144)),
%           _150144 before _149434,
%           not ((terminates_at(_152720,_149428,_152732),_150144 before _152732),_152732 before _149434))

% it becomes the case that
%   fluent
% when
%   event
% if 
% statement/1 or /3 
statement(Statement) --> 
    it_becomes_the_case_that_, spaces_or_newlines(_), 
        literal_([], Map1, holds(Fluent, _)), spaces_or_newlines(_), 
    when_, spaces_or_newlines(_), 
        literal_(Map1, Map2, happens(Event, T)), spaces_or_newlines(_),
    body_(Body, [map(T, '_change_time')|Map2],_), period,  
        {(Body = [] -> Statement = [if(initiates(Event, Fluent, T), true)]; 
            (Statement = [if(initiates(Event, Fluent, T), Body)]))}, !.

% it becomes not the case that
%   fluent
% when
%   event
% if  
statement(Statement) --> 
    it_becomes_not_the_case_that_, spaces_or_newlines(_), 
        literal_([], Map1, holds(Fluent, _)), spaces_or_newlines(_),
    when_, spaces_or_newlines(_),
        literal_(Map1, Map2, happens(Event, T)), spaces_or_newlines(_),
    body_(Body, [map(T, '_change_time')|Map2],_), period,  
        {(Body = [] -> Statement = [if(terminates(Event, Fluent, T), true)];  
            (Statement = [if(terminates(Event, Fluent, T), Body)] %, print_message(informational, "~w"-Statement)
            ))}, !.

% it is illegal that
%   event
% if ... 
statement(Statement) -->
    it_is_illegal_that_, spaces_or_newlines(_), 
    literal_([], Map1, happens(Event, T)), body_(Body, Map1, _), period,
    {(Body = [] -> Statement = [if(it_is_illegal(Event, T), true)]; 
      Statement = [if(it_is_illegal(Event, T), Body)])},!. 

% it is unknown whether 
statement(Statement) -->
    it_is_unknown_whether_, spaces_or_newlines(_), 
    literal_([], Map1, Abducible), body_(Body, Map1, _), period,
    {(Body = [] -> Statement = [abducible(Abducible, true)]; 
      Statement = [abducible(Abducible, Body)])},!.

% a fact or a rule
statement(Statement) --> currentLine(L), 
    literal_([], Map1, Head), body_(Body, Map1, _), period,  
    {(Body = [] -> Statement = [if(L, Head, true)]; Statement = [if(L, Head, Body)])}. 

% error
statement(_, Rest, _) :- 
    asserterror('LE error found around this statement: ', Rest), fail.

list_of_facts([F|R1]) --> literal_([], _,F), rest_list_of_facts(R1).

rest_list_of_facts(L1) --> comma, spaces_or_newlines(_), list_of_facts(L1).
rest_list_of_facts([]) --> [].

% assumptions_/3 or /5
assumptions_([A|R]) --> 
        spaces_or_newlines(_),  rule_([], _, A), !, assumptions_(R).
assumptions_([]) --> 
        spaces_or_newlines(_), []. 

rule_(InMap, InMap, Rule) -->
    it_is_unknown_whether_, spaces_or_newlines(_), 
    literal_([], Map1, Abducible), body_(Body, Map1, _), period,
    {(Body = [] -> Rule = (abducible(Abducible, true):-true); Rule = (abducible(Abducible, Body):-true))},!.

rule_(InMap, OutMap, Rule) --> 
    literal_(InMap, Map1, Head), body_(Body, Map1, OutMap), period,  
    %spaces(Ind), condition(Head, Ind, InMap, Map1), body_(Body, Map1, OutMap), period, 
    {(Body = [] -> Rule = (Head :-true); Rule = (Head :- Body))}. 

rule_(M, M, _, Rest, _) :- 
    asserterror('LE error found in an assumption, near to ', Rest), fail.

% no prolog inside LE!
%statement([Fact]) --> 
%    spaces(_), prolog_literal_(Fact, [], _), spaces_or_newlines(_), period.
% body/3 or /5
body_([], Map, Map) --> spaces_or_newlines(_).
body_(Conditions, Map1, MapN) --> 
    newline, spaces(Ind), if_, !, conditions(Ind, Map1, MapN, Conditions), spaces_or_newlines(_).
body_(Conditions, Map1, MapN) --> 
    if_, newline_or_nothing, spaces(Ind), conditions(Ind, Map1, MapN, Conditions), spaces_or_newlines(_).

newline_or_nothing --> newline.
newline_or_nothing --> []. 

% literal_/3 or /5
% literal_ reads a list of words until it finds one of these: ['\n', if, '.']
% it then tries to match those words against a template in memory (see dict/3 predicate).
% The output is then contigent to the type of literal according to the declarations. 
literal_(Map1, MapN, FinalLiteral) --> % { print_message(informational, 'at time, literal') },
    at_time(T, Map1, Map2), comma, possible_instance(PossibleTemplate),  
    {match_template(PossibleTemplate, Map2, MapN, Literal),
     (fluents(Fluents) -> true; Fluents = []),
     (events(Events) -> true; Events = []),
     (lists:member(Literal, Events) -> FinalLiteral = happens(Literal, T) 
      ; (lists:member(Literal, Fluents) -> FinalLiteral = holds(Literal, T)
        ; FinalLiteral = Literal))}, !. % by default (including builtins) they are timeless!

literal_(Map1, MapN, FinalLiteral) --> % { print_message(informational, 'literal, at time') },
    possible_instance(PossibleTemplate), comma, at_time(T, Map1, Map2),  
    {match_template(PossibleTemplate, Map2, MapN, Literal),
     (fluents(Fluents) -> true; Fluents = []),
     (events(Events) -> true; Events = []),
     (lists:member(Literal, Events) -> FinalLiteral = happens(Literal, T) 
      ; (lists:member(Literal, Fluents) -> FinalLiteral = holds(Literal, T)
        ; FinalLiteral = Literal))}, !. % by default (including builtins) they are timeless!

literal_(Map1, MapN, FinalLiteral) -->  
    possible_instance(PossibleTemplate), %{ print_message(informational, "~w"-[PossibleTemplate]) },
    {match_template(PossibleTemplate, Map1, MapN, Literal),
     (fluents(Fluents) -> true; Fluents = []),
     (events(Events) -> true; Events = []),
     (consult_map(Time, '_change_time', Map1, _MapF) -> T=Time; true), 
     (lists:member(Literal, Events) -> FinalLiteral = happens(Literal, T) 
      ; (lists:member(Literal, Fluents) -> FinalLiteral = holds(Literal, T)
        ; (FinalLiteral = Literal)))
      %print_message(informational, "~w with ~w"-[FinalLiteral, MapF])
     }, !. % by default (including builtins) they are timeless!

% rewritten to use in swish. Fixed! It was a name clash. Apparently "literal" is used somewhere else
%literal_(Map1, MapN, Literal, In, Out) :-  print_message(informational, '  inside a literal'),
%        possible_instance(PossibleTemplate, In, Out), print_message(informational, PossibleTemplate),
%        match_template(PossibleTemplate, Map1, MapN, Literal).
% error clause
literal_(M, M, _, Rest, _) :- 
    asserterror('LE error found in a literal ', Rest), fail.

% conditions/4 or /6
conditions(Ind0, Map1, MapN, Conds) --> 
    list_of_conds_with_ind(Ind0, Map1, MapN, Errors, ListConds),
    {Errors=[] -> ri(Conds, ListConds); (assert_error_os(Errors), fail)}. % preempty validation of errors  
conditions(_, Map, Map, _, Rest, _) :-
    asserterror('LE indentation error ', Rest), fail. 

% list_of_conds_with_ind/5
% list_of_conds_with_ind(+InitialInd, +InMap, -OutMap, -Errors, -ListOfConds)
list_of_conds_with_ind(Ind0, Map1, MapN, [], [Cond|Conditions]) -->
    condition(Cond, Ind0, Map1, Map2),
    more_conds(Ind0, Ind0,_, Map2, MapN, Conditions).
list_of_conds_with_ind(_, M, M, [error('Error in condition at', LineNumber, Tokens)], [], Rest, _) :-
    once( nth1(N,Rest,newline(NextLine)) ), LineNumber is NextLine-2,
    RelevantN is N-1,
    length(Relevant,RelevantN), append(Relevant,_,Rest),
    findall(Token, (member(T,Relevant), (T=newline(_) -> Token='\n' ; Token=T)), Tokens). 

more_conds(Ind0, _, Ind3, Map1, MapN, [ind(Ind2), Op, Cond2|RestMapped]) --> 
    newline, spaces(Ind2), {Ind0 =< Ind2}, % if the new indentation is deeper, it goes on as before. 
    operator(Op), condition(Cond2, Ind2, Map1, Map2),
    %{print_message(informational, "~w"-[Conditions])}, !,
    more_conds(Ind0, Ind2, Ind3, Map2, MapN, RestMapped). 
more_conds(_, Ind, Ind, Map, Map, [], L, L).  
 
% this naive definition of term is problematic
% term_/4 or /6
term_(StopWords, Term, Map1, MapN) --> 
    (variable(StopWords, Term, Map1, MapN), !); (constant(StopWords, Term, Map1, MapN), !); (list_(Term, Map1, MapN), !). %; (compound_(Term, Map1, MapN), !).

% list_/3 or /5
list_(List, Map1, MapN) --> 
    spaces(_), bracket_open_, !, extract_list([']'], List, Map1, MapN), bracket_close.   

compound_(V1/V2, Map1, MapN) --> 
    term_(['/'], V1, Map1, Map2), ['/'], term_([], V2, Map2, MapN). 

% event observations
%condition(happens(Event), _, Map1, MapN) -->
%    observe_,  literal_(Map1, MapN, Event), !.

% condition/4 or /6
% this produces a Taxlog condition with the form: 
% setof(Owner/Share, is_ultimately_owned_by(Asset,Owner,Share) on Before, SetOfPreviousOwners)
% from a set of word such as: 
%     and a record of previous owners is a set of [an owner, a share] 
%           where the asset is ultimately owned by the share with the owner at the previous time
condition(FinalExpression, _, Map1, MapN) --> 
    variable([is], Set, Map1, Map2), is_a_set_of_, term_([], Term, Map2, Map3), !, % moved where to the following line
    newline, spaces(Ind2), where_, conditions(Ind2, Map3, Map4, Goals),
    modifiers(setof(Term,Goals,Set), Map4, MapN, FinalExpression).

% for every a party is a party in the event, it is the case that:
condition(FinalExpression, _, Map1, MapN) -->  
    for_all_cases_in_which_, newline, !, 
    spaces(Ind2), conditions(Ind2, Map1, Map2, Conds), spaces_or_newlines(_), 
    it_is_the_case_that_, newline, 
    spaces(Ind3), conditions(Ind3, Map2, Map3, Goals),
    modifiers(forall(Conds,Goals), Map3, MapN, FinalExpression).

% the Value is the sum of each Asset Net such that
condition(FinalExpression, _, Map1, MapN) --> 
    variable([is], Value, Map1, Map2), is_the_sum_of_each_, extract_variable([such], [], NameWords, [], _), such_that_, !, 
    { name_predicate(NameWords, Name), update_map(Each, Name, Map2, Map3) }, newline, 
    spaces(Ind), conditions(Ind, Map3, Map4, Conds), 
    modifiers(aggregate_all(sum(Each),Conds,Value), Map4, MapN, FinalExpression).
    
% it is not the case that 
%condition((this_capsule(M), not(M:Conds)), _, Map1, MapN) --> 
%condition((true, not(Conds)), _, Map1, MapN) -->
condition(not(Conds), _, Map1, MapN) --> 
%condition(not(Conds), _, Map1, MapN) --> 
    spaces(_), not_, newline,  % forget other choices. We know it is a not case
    spaces(Ind), conditions(Ind, Map1, MapN, Conds), !.

condition(Cond, _, Map1, MapN) -->  
    literal_(Map1, MapN, Cond), !. 

% error clause
condition(_, _Ind, Map, Map, Rest, _) :- 
        asserterror('LE error found at a condition ', Rest), fail.

% modifiers add reifying predicates to an expression. 
% modifiers(+MainExpression, +MapIn, -MapOut, -FinalExpression)
modifiers(MainExpression, Map1, MapN, on(MainExpression, Var) ) -->
    newline, spaces(_), at_, variable([], Var, Map1, MapN). % newline before a reifying expression
modifiers(MainExpression, Map, Map, MainExpression) --> [].  

% variable/4 or /6
variable(StopWords, Var, Map1, MapN) --> 
    spaces(_), indef_determiner, extract_variable(StopWords, [], NameWords, [], _), % <-- CUT!
    {  NameWords\=[], name_predicate(NameWords, Name), update_map(Var, Name, Map1, MapN) }. 
variable(StopWords, Var, Map1, MapN) --> 
    spaces(_), def_determiner, extract_variable(StopWords, [], NameWords, [], _), % <-- CUT!
    {  NameWords\=[], name_predicate(NameWords, Name), consult_map(Var, Name, Map1, MapN) }. 
% allowing for symbolic variables: 
variable(StopWords, Var, Map1, MapN) --> 
    spaces(_), extract_variable(StopWords, [], NameWords, [], _),
    {  NameWords\=[], name_predicate(NameWords, Name), consult_map(Var, Name, Map1, MapN) }. 

% constant/4 or /6
constant(StopWords, Constant, Map, Map) -->
    extract_constant(StopWords, NameWords), { NameWords\=[], name_predicate(NameWords, Constant) }. 

% deprecated
prolog_literal_(Prolog, Map1, MapN) -->
    predicate_name_(Predicate), parentesis_open_, extract_list([], Arguments, Map1, MapN), parentesis_close_,
    {Prolog =.. [Predicate|Arguments]}.

predicate_name_(Module:Predicate) --> 
    [Module], colon_, extract_constant([], NameWords), { name_predicate(NameWords, Predicate) }, !.
predicate_name_(Predicate) --> extract_constant([], NameWords), { name_predicate(NameWords, Predicate) }.

at_time(T, Map1, MapN) --> spaces_or_newlines(_), at_, expression_(T, Map1, MapN), spaces_or_newlines(_).

spaces(N) --> [' '], !, spaces(M), {N is M + 1}.
% todo: reach out for codemirror s configuration https://codemirror.net/doc/manual.html for tabSize
spaces(N) --> ['\t'], !, spaces(M), {N is M + 4}. % counting tab as four spaces (default in codemirror)
spaces(0) --> []. 

spaces_or_newlines(N) --> [' '], !, spaces_or_newlines(M), {N is M + 1}.
spaces_or_newlines(N) --> ['\t'], !, spaces_or_newlines(M), {N is M + 4}. % counting tab as four spaces. See above
spaces_or_newlines(N) --> newline, !, spaces_or_newlines(M), {N is M + 1}. % counting \r as one space
spaces_or_newlines(0) --> [].

newline --> [newline(_Next)].

one_or_many_newlines --> newline, spaces(_), one_or_many_newlines, !. 
one_or_many_newlines --> [].

if_ --> [if], spaces_or_newlines(_).  % so that if can be written many lines away from the rest
if_ --> [se], spaces_or_newlines(_).  % italian
if_ --> [si], spaces_or_newlines(_).  % french and spanish

period --> ['.'].
comma --> [','].
colon_ --> [':'], spaces(_). 

comma_or_period --> period, !.
comma_or_period --> comma. 

and_ --> [and].
and_ --> [e].  % italian
and_ --> [et]. % french
and_ --> [y].  % spanish

or_ --> [or].
or_ --> [o].  % italian and spanish
or_ --> [ou]. % french

not_ --> [it], spaces(_), [is], spaces(_), [not], spaces(_), [the], spaces(_), [case], spaces(_), [that], spaces(_). 
not_ --> [non], spaces(_), [risulta], spaces(_), [che], spaces(_). % italian
not_ --> [ce], spaces(_), [n],[A],[est], spaces(_), [pas], spaces(_), [le], spaces(_), [cas], spaces(_), [que], spaces(_), {atom_string(A, "'")}. % french
not_ --> [no], spaces(_), [es], spaces(_), [el], spaces(_), [caso], spaces(_), [que], spaces(_).  % spanish

is_the_sum_of_each_ --> [is], spaces(_), [the], spaces(_), [sum], spaces(_), [of], spaces(_), [each], spaces(_) .
is_the_sum_of_each_ --> [è], spaces(_), [la], spaces(_), [somma], spaces(_), [di], spaces(_), [ogni], spaces(_). % italian
is_the_sum_of_each_ --> [es], spaces(_), [la], spaces(_), [suma], spaces(_), [de], spaces(_), [cada], spaces(_). % spanish
is_the_sum_of_each_ --> [est], spaces(_), [la], spaces(_), [somme], spaces(_), [de], spaces(_), [chaque], spaces(_). % french

such_that_ --> [such], spaces(_), [that], spaces(_). 
such_that_ --> [tale], spaces(_), [che], spaces(_). % italian
such_that_ --> [tel], spaces(_), [que], spaces(_).  % french
such_that_ --> [tal], spaces(_), [que], spaces(_).  % spanish

at_ --> [at], spaces(_). 
at_ --> [a], spaces(_). % italian 

minus_ --> ['-'], spaces(_).

plus_ --> ['+'], spaces(_).

divide_ --> ['/'], spaces(_).

times_ --> ['*'], spaces(_).

bracket_open_ --> [A], spaces(_), {atom_string(A, "[")}.
bracket_close --> [A], spaces(_), {atom_string(A, "]")}. 

parenthesis_open_ --> ['('], spaces(_).
parenthesis_close_ --> [A], spaces(_), {atom_string(A, ")")}. 

this_information_ --> [this], spaces(_), [information], spaces(_).

has_been_recorded_ --> [has], spaces(_), [been], spaces(_), [recorded], spaces(_).

for_all_cases_in_which_ --> spaces_or_newlines(_), [for], spaces(_), [all], spaces(_), [cases], spaces(_), [in], spaces(_), [which], spaces(_).
for_all_cases_in_which_ --> spaces_or_newlines(_), [pour], spaces(_), [tous], spaces(_), [les], spaces(_), [cas], spaces(_), [o],[ù], spaces(_).  % french 
for_all_cases_in_which_ --> spaces_or_newlines(_), [per], spaces(_), [tutti], spaces(_), [i], spaces(_), [casi], spaces(_), [in], spaces(_), [cui], spaces(_).  % italian 
for_all_cases_in_which_ --> spaces_or_newlines(_), [en], spaces(_), [todos], spaces(_), [los], spaces(_), [casos], spaces(_), [en], spaces(_), [los], spaces(_), [que], spaces(_).  % spanish 
for_all_cases_in_which_ --> spaces_or_newlines(_), [en], spaces(_), [cualquier], spaces(_), [caso], spaces(_), [en], spaces(_), [el], spaces(_), [que], spaces(_).  % spanish 

it_is_the_case_that_ --> [it], spaces(_), [is], spaces(_), [the], spaces(_), [case], spaces(_), [that], spaces(_).
it_is_the_case_that_ --> [es], spaces(_), [el], spaces(_), [caso], spaces(_), [que], spaces(_).  % spanish
it_is_the_case_that_ --> [es], spaces(_), [también], spaces(_), [el], spaces(_), [caso], spaces(_), [que], spaces(_).  % spanish
it_is_the_case_that_ --> [c], [A], [est], spaces(_), [le], spaces(_), [cas], spaces(_), [que], spaces(_), {atom_string(A, "'")}. % french
it_is_the_case_that_ --> [è], spaces(_), [provato], spaces(_), [che], spaces(_). % italian

is_a_set_of_ --> [is], spaces(_), [a], spaces(_), [set], spaces(_), [of], spaces(_). 
is_a_set_of_ --> [es], spaces(_), [un],  spaces(_), [conjunto],  spaces(_), [de], spaces(_). % spanish
is_a_set_of_ --> [est], spaces(_), [un],  spaces(_), [ensemble],  spaces(_), [de],  spaces(_). % french
is_a_set_of_ --> [est], spaces(_), [un],  spaces(_), [ensemble],  spaces(_), [de],  spaces(_). % italian

where_ --> [where], spaces(_). 
where_ --> [en], spaces(_), [donde], spaces(_). % spanish
where_ --> ['où'], spaces(_). % french  
where_ --> [dove], spaces(_). % italian
where_ --> [quando], spaces(_). % italian
where_ --> [donde], spaces(_). % spanish

scenario_ -->  spaces_or_newlines(_), ['Scenario'], !, spaces(_).
scenario_ -->  spaces_or_newlines(_), [scenario], spaces(_). % english and italian
scenario_ -->  spaces_or_newlines(_), [le], spaces(_), [scénario], spaces(_). % french
scenario_ -->  spaces_or_newlines(_), [escenario], spaces(_). % spanish

is_colon_ -->  [is], spaces(_), [':'], spaces(_).
is_colon_ -->  [es], spaces(_), [':'], spaces(_).  % spanish
is_colon_ -->  [est], spaces(_), [':'], spaces(_). % french
is_colon_ -->  [è], spaces(_), [':'], spaces(_). % italian

query_ --> spaces_or_newlines(_), ['Query'], !, spaces(_).
query_ --> spaces_or_newlines(_), [query], spaces(_).
query_ --> spaces_or_newlines(_), [la], spaces(_), [question], spaces(_). % french
query_ --> spaces_or_newlines(_), [la], spaces(_), [pregunta], spaces(_). % spanish
query_ --> spaces_or_newlines(_), [domanda], spaces(_). % italian

for_which_ --> [for], spaces(_), [which], spaces(_). 
for_which_ --> [para], spaces(_), [el], spaces(_), [cual], spaces(_). % spanish singular
for_which_ --> [pour], spaces(_), [qui], spaces(_). % french
for_which_ --> [per], spaces(_), [cui], spaces(_). % italian

query_header(Ind, Map) --> spaces(Ind), for_which_, list_of_vars([], Map), colon_, spaces_or_newlines(_).
query_header(0, []) --> []. 

list_of_vars(Map1, MapN) --> 
    extract_variable([',', and, el, et, y, ':'], [], NameWords, [], _), 
    { name_predicate(NameWords, Name), update_map(_Var, Name, Map1, Map2) },
    rest_of_list_of_vars(Map2, MapN).

rest_of_list_of_vars(Map1, MapN) --> and_or_comma_, list_of_vars(Map1, MapN).
rest_of_list_of_vars(Map, Map) --> []. 

and_or_comma_ --> [','], spaces(_). 
and_or_comma_ --> and_, spaces(_).

it_becomes_the_case_that_ --> 
    it_, [becomes], spaces(_), [the], spaces(_), [case], spaces(_), [that], spaces(_).

it_becomes_not_the_case_that_ -->
    it_, [becomes], spaces(_), [not], spaces(_), [the], spaces(_), [case], spaces(_), [that], spaces(_).
it_becomes_not_the_case_that_ -->
    it_, [becomes], spaces(_), [no], spaces(_), [longer], spaces(_), [the], spaces(_), [case], spaces(_), [that], spaces(_).

when_ --> [when], spaces(_).

it_ --> [it], spaces(_), !.
it_ --> ['It'], spaces(_). 

observe_ --> [observe], spaces(_). 

it_is_illegal_that_  -->
    it_, [is], spaces(_), [illegal], spaces(_), [that], spaces(_).

it_is_unknown_whether_ --> 
    it_, [is], spaces(_), [unknown], spaces(_), [whether], spaces(_).

it_is_unknown_whether_ --> 
    it_, [is], spaces(_), [unknown], spaces(_), [that], spaces(_).

it_is_unknown_whether_ --> 
    [non], spaces(_), [è], spaces(_), [noto], spaces(_), [se], spaces(_). % italian

/* --------------------------------------------------- Supporting code */
% indentation code
% ri/2 ri(-Conditions, +IndentedForm). 

ri(P, L) :- rinden(Q, L), c2p(Q, P).  

% rinden/2 produces the conditions from the list with the indented form. 
rinden(Q, List) :- rind(_, _, Q, List).  

rind(L, I, Q, List) :- rind_and(L, I, Q, List); rind_or(L, I, Q, List). 

rind_and(100, [], true, []). 
rind_and(100, [], Cond, [Cond]) :- simple(Cond). 
rind_and(T, [T|RestT], and(First,Rest), Final) :-
	combine(NewF, [ind(T), and|RestC], Final),
	rind(T1, Tr1, First, NewF),
	T1>T, 
	rind(Tn, Tr, Rest, RestC),
	append(Tr1, Tr, RestT), 
	right_order_and(Rest, Tn, T). 

rind_or(100, [], false, []). 
rind_or(100, [], Cond, [Cond]) :- simple(Cond).
rind_or(T, [T|RestT], or(First,Rest), Final) :-
	combine(NewF, [ind(T), or|RestC], Final), 
	rind(T1, Tr1, First, NewF),
	T1>T, 
	rind(Tn, Tr, Rest, RestC),
	append(Tr1, Tr, RestT), 
	right_order_or(Rest, Tn, T).    
	
right_order_and(Rest, Tn, T) :- Rest=or(_,_), Tn>T. 
right_order_and(Rest, Tn, T) :- Rest=and(_,_), Tn=T.
right_order_and(Rest, _, _) :- simple(Rest).  

right_order_or(Rest, Tn, T) :- Rest=and(_,_), Tn>T. 
right_order_or(Rest, Tn, T) :- Rest=or(_,_), Tn=T.
right_order_or(Rest, _, _) :- simple(Rest).  

combine(F, S, O) :- ( F\=[], S=[ind(_), Op, V], ((Op==and_); (Op==or_)), simple(V), O=F) ; (F=[], O=S). 
combine([H|T], S, [H|NT]) :- combine(T, S, NT). 

simple(Cond) :- Cond\=and(_,_), Cond\=or(_,_), Cond\=true, Cond\=false.  

c2p(true, true).
c2p(false, false). 
c2p(C, C) :- simple(C). 
c2p(and(A, RestA), (AA, RestAA)) :- 
	c2p(A, AA), 
	c2p(RestA, RestAA). 
c2p(or(A, RestA), (AA; RestAA)) :- 
	c2p(A, AA), 
	c2p(RestA, RestAA). 

/* --------------------------------------------------- More Supporting code */
clean_comments([], []) :- !.
clean_comments(['%'|Rest], New) :- % like in prolog comments start with %
    jump_comment(Rest, Next), 
    clean_comments(Next, New). 
clean_comments([Code|Rest], [Code|New]) :-
    clean_comments(Rest, New).

jump_comment([], []).
jump_comment([newline(N)|Rest], [newline(N)|Rest]). % leaving the end of line in place
jump_comment([_|R1], R2) :-
    jump_comment(R1, R2). 

% template_decl/4
% cuts added to improve efficiency
template_decl([], [newline(_)|RestIn], [newline(_)|RestIn]) :- 
    asserterror('LE error: misplaced new line found in a template declaration ', RestIn), !, 
    fail. % cntrl \n should be rejected as part of a template
template_decl(RestW, [' '|RestIn], Out) :- !, % skip spaces in template
    template_decl(RestW, RestIn, Out).
template_decl(RestW, ['\t'|RestIn], Out) :- !, % skip cntrl \t in template
    template_decl(RestW, RestIn, Out).
% excluding ends of lines from templates
%template_decl(RestW, [newline(_)|RestIn], Out) :- !, % skip cntrl \n in template
%    template_decl(RestW, RestIn, Out).
template_decl([Word|RestW], [Word|RestIn], Out) :-
    not(lists:member(Word,['.', ','])),   % only . and , as boundaries. Beware!
    template_decl(RestW, RestIn, Out), !.
template_decl([], [Word|Rest], [Word|Rest]) :-
    lists:member(Word,['.', ',']), !.
template_decl(_, Rest, _) :- 
    asserterror('LE error found in a template declaration ', Rest), fail.

% build_template/5
build_template(RawTemplate, Predicate, Arguments, TypesAndNames, Template) :-
    build_template_elements(RawTemplate, [], Arguments, TypesAndNames, OtherWords, Template),
    name_predicate(OtherWords, Predicate).

% build_template_elements(+Input, +Previous, -Args, -TypesNames, -OtherWords, -Template)
build_template_elements([], _, [], [], [], []) :- !. 
% a variable signalled by a *
build_template_elements(['*', Word|RestOfWords], _Previous, [Var|RestVars], [Name-Type|RestTypes], Others, [Var|RestTemplate]) :-
    has_pairing_asteriks([Word|RestOfWords]), 
    %(ind_det(Word); ind_det_C(Word)), % Previous \= [is|_], % removing this requirement when * is used
    phrase(determiner, [Word|RestOfWords], RRestOfWords), % allows the for variables in templates declarations only
    extract_variable_template(['*'], [], NameWords, [], TypeWords, RRestOfWords, ['*'|NextWords]), !, % <-- it must end with * too
    name_predicate(NameWords, Name),
    name_predicate(TypeWords, Type), 
    build_template_elements(NextWords, [], RestVars, RestTypes, Others, RestTemplate). 
build_template_elements(['*', Word|RestOfWords], _Previous,_, _, _, _) :-
    not(has_pairing_asteriks([Word|RestOfWords])), !, fail. % produce an error report if asterisks are not paired
% a variable not signalled by a *  % for backward compatibility  \\ DEPRECATED
%build_template_elements([Word|RestOfWords], Previous, [Var|RestVars], [Name-Type|RestTypes], Others, [Var|RestTemplate]) :-
%    (ind_det(Word); ind_det_C(Word)), Previous \= [is|_], 
%    extract_variable(['*'], Var, [], NameWords, TypeWords, RestOfWords, NextWords), !, % <-- CUT!
%    name_predicate(NameWords, Name), 
%    name_predicate(TypeWords, Type), 
%    build_template_elements(NextWords, [], RestVars, RestTypes, Others, RestTemplate).
build_template_elements([Word|RestOfWords], Previous, RestVars, RestTypes,  [Word|Others], [Word|RestTemplate]) :-
    build_template_elements(RestOfWords, [Word|Previous], RestVars, RestTypes, Others, RestTemplate).

has_pairing_asteriks(RestOfTemplate) :-
    findall('*',member('*', RestOfTemplate), Asteriks), length(Asteriks, N), 1 is mod(N, 2).

name_predicate(Words, Predicate) :-
    concat_atom(Words, '_', Predicate). 

% name_as_atom/2
name_as_atom([Number], Number) :-
    number(Number), !. 
name_as_atom([Atom], Number) :- 
    atom_number(Atom, Number), !. 
name_as_atom(Words, Name) :-
    numbervars(Words, 1, _, [functor_name('unknown')]),
    replace_vars(Words, Atoms), 
    list_words_to_codes(Atoms, Codes),
    replace_ast_a(Codes, CCodes), 
    atom_codes(Name, CCodes).  

words_to_atom(Words, Name) :- %trace, 
    numbervars(Words, 0, _, [singletons(true)]),
    list_words_to_codes(Words, Codes),
    atom_codes(Name, Codes). 

replace_ast_a([], []) :- !. 
replace_ast_a([42,32,97|Rest], [42,97|Out]) :- !, 
    replace_final_ast(Rest, Out). 
replace_ast_a([C|Rest], [C|Out]) :-
    replace_ast_a(Rest, Out).

replace_final_ast([], []) :- !. 
replace_final_ast([32,42|Rest], [42|Out]) :- !, 
    replace_ast_a(Rest, Out).
replace_final_ast([C|Rest], [C|Out]) :-
    replace_final_ast(Rest, Out).

% maps a list of words to a list of corresponding codes
% adding an space between words-codes (32). 
% list_word_to_codes/2
list_words_to_codes([], []).
list_words_to_codes([Word|RestW], Out) :-
    atom_codes(Word, Codes),
    remove_quotes(Codes, CleanCodes), 
    list_words_to_codes(RestW, Next),
    (Next=[]-> Out=CleanCodes;
        % if it comes the symbol _ + - / \ or the previous is only + o - then no space is added between words
        (Next=[95|_]; Next=[43|_]; Next=[45|_]; Next=[47|_]; Next=[92|_];
         CleanCodes=[43]; CleanCodes=[45])->  
            append(CleanCodes, Next, Out);
            append(CleanCodes, [32|Next], Out)
    ), !. 

remove_quotes([], []) :-!.
remove_quotes([39|RestI], RestC) :- remove_quotes(RestI, RestC), !.
% quick fix to remove parentheses and numbers too. 
remove_quotes([40, _, 41|RestI], RestC) :- remove_quotes(RestI, RestC), !.
%remove_quotes([41|RestI], RestC) :- remove_quotes(RestI, RestC), !.
remove_quotes([C|RestI], [C|RestC]) :- remove_quotes(RestI, RestC). 

replace_vars([],[]) :- !.
replace_vars([A|RI], [A|RO]) :- atom(A), replace_vars(RI,RO), !.
replace_vars([W|RI], [A|RO]) :- term_to_atom(W, A), replace_vars(RI,RO).   

add_cond(and, Ind1, Ind2, Previous, C4, (C; (C3, C4))) :-
    last_cond(or, Previous, C, C3), % (C; C3)
    Ind1 < Ind2, !. 
add_cond(and, Ind1, Ind2, Previous, C4, ((C; C3), C4)) :-
    last_cond(or, Previous, C, C3), % (C; C3)
    Ind1 > Ind2, !.     
add_cond(and,I, I, (C, C3), C4, (C, (C3, C4))) :- !. 
add_cond(and,_, _, Cond, RestC, (Cond, RestC)) :- !. 
add_cond(or, Ind1, Ind2, Previous, C4, (C, (C3; C4))) :- 
    last_cond(and, Previous, C, C3),  % (C, C3)
    Ind1 < Ind2, !. 
add_cond(or, Ind1, Ind2, Previous, C4, ((C, C3); C4)) :- 
    last_cond(and, Previous, C, C3), % (C, C3)
    Ind1 > Ind2, !. 
add_cond(or, I, I, (C; C3), C4, (C; (C3; C4))) :- !. 
add_cond(or, _, _, Cond, RestC, (Cond; RestC)).

last_cond(or, (A;B), A, B) :- B\=(_;_), !.
last_cond(or, (C;D), (C;R), Last) :- last_cond(or, D, R, Last).

last_cond(and, (A,B), A, B) :- B\=(_,_), !.
last_cond(and, (C,D), (C,R), Last) :- last_cond(and, D, R, Last).

% adjust_op(Ind1, Ind2, PreviousCond, Op1, Cond2, Op2, Rest, RestMapped, Conditions)
% from and to and
adjust_op(Ind1, Ind2, C1, and, C2, and, C3, ((C1, C2), C3) ) :- 
    Ind1 =< Ind2, !.
adjust_op(Ind1, Ind2, C1, and, C2, and, C3, ((C1, C2), C3) ) :- 
    Ind1 > Ind2, !.
% from or to ord
adjust_op(Ind1, Ind2, C1, or, C2, or, C3, ((C1; C2); C3) ) :- 
    Ind1 =< Ind2, !.
adjust_op(Ind1, Ind2, C1, or, C2, or, C3, ((C1; C2); C3) ) :- 
    Ind1 > Ind2, !.
% from and to deeper or
adjust_op(Ind1, Ind2, C1, and, C2, or, C3, (C1, (C2; C3)) ) :- 
    Ind1 < Ind2, !.
% from deeper or to and
adjust_op(Ind1, Ind2, C1, or, C2, and, C3, ((C1; C2), C3) ) :- 
    Ind1 > Ind2, !.
% from or to deeper and
adjust_op(Ind1, Ind2, C1, or, C2, and, C3, (C1; (C2, C3)) ) :- 
    Ind1 < Ind2, !.
% from deeper and to or
adjust_op(Ind1, Ind2, C1, and, C2, or, C3, ((C1, C2); C3) ) :- 
    Ind1 > Ind2.

operator(and, In, Out) :- and_(In, Out).
operator(or, In, Out) :- or_(In, Out).

% possible_instance/3
% cuts added to improve efficiency
% skipping a list
possible_instance([], [], []) :- !. 
possible_instance(Final, ['['|RestIn], Out) :- !, 
    possible_instance_for_lists(List, RestIn, [']'|Next]),  
    possible_instance(RestW, Next, Out),
    append(['['|List], [']'|RestW], Final).  
possible_instance(RestW, [' '|RestIn], Out) :- !, % skip spaces in template
    possible_instance(RestW, RestIn, Out).
possible_instance(RestW, ['\t'|RestIn], Out) :- !, % skip tabs in template
    possible_instance(RestW, RestIn, Out).
possible_instance([that|Instance], In, Out) :- % to allow "that" instances to spread over more than one line
    phrase(spaces_or_newlines(_), In, [that|Rest]),
    phrase(spaces_or_newlines(_), Rest, Next), !, 
    possible_instance(Instance, Next, Out).
possible_instance([Word|RestW], [Word|RestIn], Out) :- 
    %not(lists:member(Word,['\n', if, and, or, '.', ','])),  !, 
    not(lists:member(Word,[newline(_), if, '.', ','])), 
    % leaving the comma in as well (for lists and sets we will have to modify this)
    possible_instance(RestW, RestIn, Out).
possible_instance([], [Word|Rest], [Word|Rest]) :- 
    lists:member(Word,[newline(_), if, '.', ',']). % leaving or/and out of this

% using [ and ] for list and set only to avoid clashes for commas
%possible_instance_for_lists([], [], []) :- !.
possible_instance_for_lists([], [']'|Out], [']'|Out]) :- !. 
possible_instance_for_lists(RestW, [' '|RestIn], Out) :- !, % skip spaces in template
    possible_instance_for_lists(RestW, RestIn, Out).
possible_instance_for_lists(RestW, ['\t'|RestIn], Out) :- !, % skip tabs in template
    possible_instance_for_lists(RestW, RestIn, Out).
possible_instance_for_lists([Word|RestW], [Word|RestIn], Out) :- 
    %not(lists:member(Word,['\n', if, and, or, '.', ','])),  !, 
    possible_instance_for_lists(RestW, RestIn, Out).
%possible_instance_for_lists([], [Word|Rest], [Word|Rest]) :- 
%    lists:member(Word,[',', newline(_), if, '.']). % leaving or/and out of this

% match_template/4
match_template(PossibleLiteral, Map1, MapN, Literal) :-
    %print_message(informational,'Possible Meta Literal ~w'-[PossibleLiteral]),
    meta_dictionary(Predicate, _, MetaCandidate),
    meta_match(MetaCandidate, PossibleLiteral, Map1, MapN, MetaTemplate), !, 
    meta_dictionary(Predicate, _, MetaTemplate),
    Literal =.. Predicate. 

match_template(PossibleLiteral, Map1, MapN, Literal) :- 
    %print_message(informational,'Possible Literal ~w'-[PossibleLiteral]),
    dictionary(Predicate, _, Candidate),
    match(Candidate, PossibleLiteral, Map1, MapN, Template), !, 
    dictionary(Predicate, _, Template), 
    Literal =.. Predicate.
    %print_message(informational,'Match!! with ~w'-[Literal]).% !. 

% meta_match/5
% meta_match(+CandidateTemplate, +PossibleLiteral, +MapIn, -MapOut, -SelectedTemplate)
meta_match([], [], Map, Map, []) :- !.
meta_match([Word|_LastElement], [Word|PossibleLiteral], Map1, MapN, [Word,Literal]) :- % asuming Element is last in template!
    Word = that, % that is a reserved word "inside" templates! -> <meta level> that <object level> 
    (meta_dictionary(Predicate, _, Candidate); dictionary(Predicate, _, Candidate)), % searching for a new inner literal
    match(Candidate, PossibleLiteral, Map1, MapN, InnerTemplate),
    (meta_dictionary(Predicate, _, InnerTemplate); dictionary(Predicate, _, InnerTemplate)), 
    Literal =.. Predicate, !. 
meta_match([MetaElement|RestMetaElements], [MetaWord|RestPossibleLiteral], Map1, MapN, [MetaElement|RestSelected]) :-
    nonvar(MetaElement), MetaWord = MetaElement, !, 
    meta_match(RestMetaElements, RestPossibleLiteral, Map1, MapN, RestSelected).
%meta_match([MetaElement|RestMetaElements], PossibleLiteral, Map1, MapN, [Literal|RestSelected]) :-
%    var(MetaElement), stop_words(RestMetaElements, StopWords), 
%    extract_literal(StopWords, LiteralWords, PossibleLiteral, NextWords),
%    meta_dictionary(Predicate, _, Candidate),
%    match(Candidate, LiteralWords, Map1, Map2, Template),  %only two meta levels! % does not work. 
%    meta_dictionary(Predicate, _, Template), 
%    Literal =.. Predicate, !, 
%    meta_match(RestMetaElements, NextWords, Map2, MapN, RestSelected).  
meta_match([MetaElement|RestMetaElements], PossibleLiteral, Map1, MapN, [Literal|RestSelected]) :-
    var(MetaElement), stop_words(RestMetaElements, StopWords), 
    extract_literal(StopWords, LiteralWords, PossibleLiteral, NextWords),
    dictionary(Predicate, _, Candidate), % this assumes that the "contained" literal is an object level literal. 
    match(Candidate, LiteralWords, Map1, Map2, Template), 
    dictionary(Predicate, _, Template), 
    Literal =.. Predicate, !, 
    meta_match(RestMetaElements, NextWords, Map2, MapN, RestSelected).  
% it could also be an object level matching of other kind
meta_match([Element|RestElements], [Det|PossibleLiteral], Map1, MapN, [Var|RestSelected]) :-
    var(Element), 
    phrase(indef_determiner, [Det|PossibleLiteral], RPossibleLiteral), stop_words(RestElements, StopWords), 
    extract_variable(StopWords, [], NameWords, [], _, RPossibleLiteral, NextWords),  NameWords \= [], % <- leave that _ unbound!
    name_predicate(NameWords, Name), 
    update_map(Var, Name, Map1, Map2), !,  % <-- CUT!  
    meta_match(RestElements, NextWords, Map2, MapN, RestSelected). 
meta_match([Element|RestElements], [Det|PossibleLiteral], Map1, MapN, [Var|RestSelected]) :-
    var(Element), 
    phrase(def_determiner, [Det|PossibleLiteral], RPossibleLiteral), stop_words(RestElements, StopWords), 
    extract_variable(StopWords, [], NameWords, [], _, RPossibleLiteral, NextWords),  NameWords \= [], % <- leave that _ unbound!
    name_predicate(NameWords, Name), 
    consult_map(Var, Name, Map1, Map2), !,  % <-- CUT!  
    meta_match(RestElements, NextWords, Map2, MapN, RestSelected). 
% handling symbolic variables (as long as they have been previously defined and included in the map!) 
meta_match([Element|RestElements], PossibleLiteral, Map1, MapN, [Var|RestSelected]) :-
    var(Element), stop_words(RestElements, StopWords), 
    extract_variable(StopWords, [], NameWords, [], _, PossibleLiteral, NextWords),  NameWords \= [], % <- leave that _ unbound!
    name_predicate(NameWords, Name), 
    consult_map(Var, Name, Map1, Map2), !, % <-- CUT!  % if the variables has been previously registered
    meta_match(RestElements, NextWords, Map2, MapN, RestSelected).
meta_match([Element|RestElements], ['['|PossibleLiteral], Map1, MapN, [List|RestSelected]) :-
    var(Element), stop_words(RestElements, StopWords),
    extract_list([']'|StopWords], List, Map1, Map2, PossibleLiteral, [']'|NextWords]), !, % matching brackets verified
    meta_match(RestElements, NextWords, Map2, MapN, RestSelected).
% enabling expressions and constants
meta_match([Element|RestElements], [Word|PossibleLiteral], Map1, MapN, [Expression|RestSelected]) :-
    var(Element), stop_words(RestElements, StopWords),
    extract_expression([','|StopWords], NameWords, [Word|PossibleLiteral], NextWords), NameWords \= [],
    % this expression cannot add variables 
    ( phrase(expression_(Expression, Map1, Map1), NameWords) -> true ; ( name_predicate(NameWords, Expression) ) ),
    %print_message(informational, 'found a constant or an expression '), print_message(informational, Expression),
    meta_match(RestElements, NextWords, Map1, MapN, RestSelected). 

% match/5
% match(+CandidateTemplate, +PossibleLiteral, +MapIn, -MapOut, -SelectedTemplate)
match([], [], Map, Map, []) :- !.  % success! It succeds iff PossibleLiteral is totally consumed
% meta level access: that New Literal
match([Word|_LastElement], [Word|PossibleLiteral], Map1, MapN, [Word,Literal]) :- % asuming Element is last in template!
    Word = that, % that is a reserved word "inside" templates! -> <meta level> that <object level> 
    (meta_dictionary(Predicate, _, Candidate); dictionary(Predicate, _, Candidate)), % searching for a new inner literal
    match(Candidate, PossibleLiteral, Map1, MapN, InnerTemplate),
    (meta_dictionary(Predicate, _, InnerTemplate); dictionary(Predicate, _, InnerTemplate)), 
    Literal =.. Predicate, !. 
%match([Element, Apost|RestElements], [_Word|PossibleLiteral], Map1, MapN, [Element, Apost|RestSelected]) :-
%    nonvar(Element), atom_string(Apost, "'"), !, %Word aprox= Element, TO BE DONE: full test
%    match(RestElements, PossibleLiteral, Map1, MapN, RestSelected). 
%match([Element|RestElements], [_Word, Apost|PossibleLiteral], Map1, MapN, [Element|RestSelected]) :-
%    nonvar(Element), atom_string(Apost, "'"), !, %Word aprox= Element, TO BE DONE: full test
%    match(RestElements, PossibleLiteral, Map1, MapN, RestSelected). 
match([Element|RestElements], [Word|PossibleLiteral], Map1, MapN, [Element|RestSelected]) :-
    nonvar(Element), Word = Element, 
    match(RestElements, PossibleLiteral, Map1, MapN, RestSelected). 
match([Element|RestElements], [Det|PossibleLiteral], Map1, MapN, [Var|RestSelected]) :-
    var(Element), 
    phrase(indef_determiner,[Det|PossibleLiteral], RPossibleLiteral), stop_words(RestElements, StopWords), 
    extract_variable(StopWords, [], NameWords, [], _, RPossibleLiteral, NextWords),  NameWords \= [], % <- leave that _ unbound!
    name_predicate(NameWords, Name), 
    update_map(Var, Name, Map1, Map2), !,  % <-- CUT!  
    match(RestElements, NextWords, Map2, MapN, RestSelected). 
match([Element|RestElements], [Det|PossibleLiteral], Map1, MapN, [Var|RestSelected]) :-
    var(Element), 
    phrase(def_determiner, [Det|PossibleLiteral], RPossibleLiteral), stop_words(RestElements, StopWords), 
    extract_variable(StopWords, [], NameWords, [], _, RPossibleLiteral, NextWords),  NameWords \= [], % <- leave that _ unbound!
    name_predicate(NameWords, Name), 
    consult_map(Var, Name, Map1, Map2), !,  % <-- CUT!  
    match(RestElements, NextWords, Map2, MapN, RestSelected). 
% handling symbolic variables (as long as they have been previously defined and included in the map!) 
match([Element|RestElements], PossibleLiteral, Map1, MapN, [Var|RestSelected]) :-
    var(Element), stop_words(RestElements, StopWords), 
    extract_variable(StopWords, [], NameWords, [], _, PossibleLiteral, NextWords),  NameWords \= [], % <- leave that _ unbound!
    name_predicate(NameWords, Name), 
    consult_map(Var, Name, Map1, Map2), !, % <-- CUT!  % if the variables has been previously registered
    match(RestElements, NextWords, Map2, MapN, RestSelected).
match([Element|RestElements], ['['|PossibleLiteral], Map1, MapN, [List|RestSelected]) :-
    var(Element), stop_words(RestElements, StopWords),
    extract_list([']'|StopWords], List, Map1, Map2, PossibleLiteral, [']'|NextWords]),  % matching brackets verified
    %print_message(informational, "List ~w"-[List]),  
    %correct_list(List, Term), 
    match(RestElements, NextWords, Map2, MapN, RestSelected).
% enabling expressions and constants
match([Element|RestElements], [Word|PossibleLiteral], Map1, MapN, [Expression|RestSelected]) :-
    var(Element), stop_words(RestElements, StopWords),
    %print_message(informational, [Word|PossibleLiteral]),
    extract_expression([','|StopWords], NameWords, [Word|PossibleLiteral], NextWords), NameWords \= [],
    % print_message(informational, "Expression? ~w"-[NameWords]),
    % this expression cannot add variables 
    ( phrase(expression_(Expression, Map1, _), NameWords) -> true ; ( name_predicate(NameWords, Expression) ) ),
    %print_message(informational, 'found a constant or an expression '), print_message(informational, Expression),
    match(RestElements, NextWords, Map1, MapN, RestSelected). 

correct_list([], []) :- !. 
correct_list([A,B], [A,B]) :- atom(B), !. % not(is_list(B)), !. 
correct_list([A,B], [A|B] ) :- !. 
correct_list([A|B], [A|NB]) :- correct_list(B, NB). 

% expression/3 or /5
%expression_(List, MapIn, MapOut) --> list_(List, MapIn, MapOut), !. 
% expression_ resolve simple math (non boolean) expressions fttb. 
% dates must be dealt with first  
% 2021-02-06T08:25:34 is transformed into 1612599934.0.
expression_(DateInSeconds, Map, Map) --> 
    [Year,'-', Month, '-', DayTHours,':', Minutes, ':', Seconds], spaces(_),
    { concat_atom([Year,'-', Month, '-', DayTHours,':', Minutes, ':', Seconds], '', Date), 
      parse_time(Date,DateInSeconds) %, print_message(informational, "~w"-[DateInSeconds])  
    }, !.
% 2021-02-06
expression_(DateInSeconds, Map, Map) -->  [Year,'-', Month, '-', Day],  spaces(_),
    { concat_atom([Year, Month, Day], '', Date), parse_time(Date, DateInSeconds) }, !. 
% basic float  extracted from atoms from the tokenizer
%expression_(Float, Map, Map) --> [AtomNum,'.',AtomDecimal],
%        { atom(AtomNum), atom(AtomDecimal), atomic_list_concat([AtomNum,'.',AtomDecimal], Atom), atom_number(Atom, Float) }, !.
expression_(Number, Map, Map) --> [Number], {number(Number)}. 
% mathematical expressions
expression_(InfixBuiltIn, Map1, MapN) --> 
    {op_stop(Stop)}, %{print_message(informational, "Stop at ~w"-[Stop])},
    term_(Stop, Term, Map1, Map2), spaces(_), 
    binary_op(BuiltIn),
    spaces(_), expression_(Expression, Map2, MapN), spaces(_), 
    !,  
    %{print_message(informational, "Term ~w and BuiltIn ~w and Expression ~w"-[Term, BuiltIn, Expression])},
    { InfixBuiltIn=..[BuiltIn, Term, Expression] }. %,  
    %print_message(informational, " ~w  ~w  ~w with map ~w "-[Term, BuiltIn, Expression, MapN]) }. % , 
% signed Value
expression_(SignedExpression, Map1, MapN) -->  % disregarding + for the time being
    %{print_message(informational, " minus something ~w"-[Map1])}, 
    minus_, spaces(_), expression_(Expression, Map1, MapN), spaces(_), !,
    {SignedExpression =.. [(-), Expression]}. 
% parentheses expression
expression_(Expression, Map1, MapN) --> 
    parenthesis_open_, spaces(_), expression_(Expression, Map1, MapN), spaces(_), parenthesis_close_, !. %,{print_message(informational, " parentheses (~w)"-[Expression])}.      
% a quick fix for integer numbers extracted from atoms from the tokenizer
expression_(Number, Map, Map) --> [Atom],  spaces(_), { atom(Atom), atom_number(Atom, Number) }, !. 
expression_(Var, Map1, Map2) -->  {op_stop(Stop)}, variable(Stop, Var, Map1, Map2),!.
expression_(Constant, Map1, Map2) -->  {op_stop(Stop)}, constant(Stop, Constant, Map1, Map2). %, {print_message(informational, "Constant Expression ~w"-Constant)}.     
% error clause
expression_(_, _, _, Rest, _) :- 
    asserterror('LE error found in an expression ', Rest), fail.

% operators with any amout of words/symbols
% binary_op/3
binary_op(Op, In, Out) :-
    op2tokens(Op, OpTokens, _),
    append(OpTokens, Out, In).

% very inefficient. Better to compute and store. See below
op_tokens(Op, OpTokens) :-
    current_op(_Prec, Fix, Op), Op \= '.', % Regenerate response
    (Fix = 'xfx'; Fix='yfx'; Fix='xfy'; Fix='yfy'),
    term_string(Op, OpString), tokenize(OpString, Tokens, [cased(true), spaces(true), numbers(false)]),
    unpack_tokens(Tokens, OpTokens).

% findall(op2tokens(Op, OpTokens, OpTokens), op_tokens(Op, OpTokens), L), forall(member(T,L), (write(T),write('.'), nl)).
% op2tokens(+Operator, PrologTokens, sCASPTokens)
% op2tokens/3
% disengaging any expression seemingly in natural language
%op2tokens(is_not_before,[is_not_before],[is_not_before]).
%op2tokens(of,[of],[of]).
%op2tokens(if,[if],[if]).
%op2tokens(then,[then],[then]).
%op2tokens(must,[must],[must]).
%op2tokens(on,[on],[on]).
%op2tokens(because,[because],[because]).
%op2tokens(and,[and],[and]).
%op2tokens(in,[in],[in]).
%op2tokens(or,[or],[or]).
%op2tokens(at,[at],[at]).
%op2tokens(before,[before],[before]).
%op2tokens(after,[after],[after]).
%op2tokens(else,[else],[else]).
%op2tokens(with,[with],[with]).
op2tokens(::,[:,:],[:,:]).
op2tokens(->,[-,>],[-,>]).
op2tokens(:,[:],[:]).
%op2tokens(,,[',,,'],[',,,']).
op2tokens(:=,[:,=],[:,=]).
op2tokens(==,[=,=],[=,=]).
op2tokens(:-,[:,-],[:,-]).
op2tokens(/\,[/,\],[/,\]).
op2tokens(=,[=],[=]).
%op2tokens(rem,[rem],[rem]).
%op2tokens(is,[is],[is]).
op2tokens(=:=,[=,:,=],[=,:,=]).
op2tokens(=\=,[=,\,=],[=,\,=]).
op2tokens(xor,[xor],[xor]).
%op2tokens(as,[as],[as]).
op2tokens(rdiv,[rdiv],[rdiv]).
op2tokens(>=,[>,=],[>,=]).
op2tokens(@<,[@,<],[@,<]).
op2tokens(@=<,[@,=,<],[@,=,<]).
op2tokens(=@=,[=,@,=],[=,@,=]).
op2tokens(\=@=,[\,=,@,=],[\,=,@,=]).
op2tokens(@>,[@,>],[@,>]).
op2tokens(@>=,[@,>,=],[@,>,=]).
op2tokens(\==,[\,=,=],[\,=,=]).
op2tokens(\=,[\,=],[\,=]).
op2tokens(>,[>],[>]).
%op2tokens(|,[',|,'],[',|,']).
op2tokens('|',['|'],['|']).
op2tokens(\/,[\,/],[\,/]).
op2tokens(+,[+],[+]).
op2tokens(>>,[>,>],[>,>]).
op2tokens(;,[;],[;]).
op2tokens(<<,[<,<],[<,<]).
op2tokens(:<,[:,<],[:,<]).
op2tokens(>:<,[>,:,<],[>,:,<]).
op2tokens(/,[/],[/]).
op2tokens(=>,[=,>],[=,>]).
op2tokens(=..,[=,.,.],[=,.,.]).
op2tokens(div,[div],[div]).
op2tokens(//,[/,/],[/,/]).
op2tokens(**,[*,*],[*,*]).
op2tokens(*,[*],[*]).
op2tokens(^,[^],[^]).
op2tokens(mod,[mod],[mod]).
op2tokens(-,[-],[-]).
op2tokens(*->,[*,-,>],[*,-,>]).
op2tokens(<,[<],[<]).
op2tokens(=<,[=,<],[=,<]).
op2tokens(-->,[-,-,>],[-,-,>]).

% very inefficient. Better to compute and store. See below
op_stop_words(Words) :-
    op_stop(Words) -> true; (    
        findall(Word, 
            (current_op(_Prec, _, Op), Op \= '.', % dont include the period!
            term_string(Op, OpString), 
            tokenize(OpString, Tokens, [cased(true), spaces(true), numbers(false)]),
            unpack_tokens(Tokens, [Word|_])), Words), % taking only the first word as stop word 
        assertz(op_stop(Words))
        ), !. 

% disengaging any word or phrase in natural language
op_stop([ 
        %(on), 
        %(because),
        %(is_not_before),
        %(not),
        %(before),
        %(and),
        %(or),
        %(at),
        (html_meta),
        %(after),
        %(in),
        %(else),
        (+),
        %(then),
        %(must),
        %(if),
        ($),
        (\),
        (=),
        (thread_initialization),
        (:),
        (\),
        '\'',
        (xor),
        (:),
        (rem),
        (\),
        %(table),
        %(initialization),
        (rdiv),
        (/),
        (>),
        (>),
        (=),
        (=),
        (;),
        %(as),
        %(is),
        (=),
        @,
        (\),
        (thread_local),
        (>),
        (=),
        (<),
        (*),
        '\'',
        (=),
        (\),
        (+),
        (:),
        (>),
        (div),
        %(discontiguous),
        (<),
        (/),
        %(meta_predicate),
        (=),
        (-),
        %(volatile),
        %(public),
        (:),
        (*),
        ?,
        (/),
        (*),
        (-),
        %(multifile),
        %(dynamic),
        (mod),
        (^)
        %(module_transparent)
      ]).

stop_words([], []).
stop_words([Word|_], [Word]) :- nonvar(Word). % only the next word for now
stop_words([Word|_], []) :- var(Word).

% list_symbol/1: a symbol specific for list that can be used as stop word for others
list_symbol('[').
list_symbol(']'). 

parenthesis('(').
parenthesis(')'). 

extract_literal(_, [], [], []) :- !. 
extract_literal(StopWords, [],  [Word|RestOfWords],  [Word|RestOfWords]) :-
    (member(Word, StopWords); that_(Word); phrase(newline, [Word])), !. 
extract_literal(SW, RestName, [' '|RestOfWords],  NextWords) :- !, % skipping spaces
    extract_literal(SW, RestName, RestOfWords, NextWords).
extract_literal(SW, RestName, ['\t'|RestOfWords],  NextWords) :- !, 
    extract_literal(SW, RestName, RestOfWords, NextWords).
extract_literal(SW, [Word|RestName], [Word|RestOfWords],  NextWords) :-
    extract_literal(SW, RestName, RestOfWords, NextWords).

% extract_variable_template/7
% extract_variable_template(+StopWords, +InitialNameWords, -FinalNameWords, +InitialTypeWords, -FinalTypeWords, +ListOfWords, -NextWordsInText)
% refactored as a dcg predicate
extract_variable_template(_, Names, Names, Types, Types, [], []) :- !.                                % stop at when words run out
extract_variable_template(StopWords, Names, Names, Types, Types, [Word|RestOfWords], [Word|RestOfWords]) :-   % stop at reserved words, verbs or prepositions. 
    %(member(Word, StopWords); reserved_word(Word); verb(Word); preposition(Word); punctuation(Word); phrase(newline, [Word])), !.  % or punctuation
    (member(Word, StopWords); that_(Word); list_symbol(Word); punctuation(Word); phrase(newline, [Word])), !.
extract_variable_template(SW, InName, OutName, InType, OutType, [' '|RestOfWords], NextWords) :- !, % skipping spaces
    extract_variable_template(SW, InName, OutName, InType, OutType, RestOfWords, NextWords).
extract_variable_template(SW, InName, OutName, InType, OutType, ['\t'|RestOfWords], NextWords) :- !, % skipping spaces
    extract_variable_template(SW, InName, OutName, InType, OutType, RestOfWords, NextWords).  
extract_variable_template(SW, InName, OutName, InType, OutType, [Word|RestOfWords], NextWords) :- % ordinals are not part of the type
    ordinal(Word), !, 
    extract_variable_template(SW, [Word|InName], OutName, InType, OutType, RestOfWords, NextWords).
%extract_variable_template(SW, InName, OutName, InType, OutType, [Word|RestOfWords], NextWords) :- % types are not part of the name
%    is_a_type(Word),
%    extract_variable(SW, InName, NextName, InType, OutType, RestOfWords, NextWords),
%    (NextName = [] -> OutName = [Word]; OutName = NextName), !.
extract_variable_template(SW, InName, [Word|OutName], InType, [Word|OutType], [Word|RestOfWords], NextWords) :- % everything else is part of the name (for instances) and the type (for templates)
    extract_variable_template(SW, InName, OutName, InType, OutType, RestOfWords, NextWords).

% extract_variable/7
% extract_variable(+StopWords, +InitialNameWords, -FinalNameWords, +InitialTypeWords, -FinalTypeWords, +ListOfWords, -NextWordsInText)
% refactored as a dcg predicate
extract_variable(_, Names, Names, Types, Types, [], []) :- !.                                % stop at when words run out
extract_variable(StopWords, Names, Names, Types, Types, [Word|RestOfWords], [Word|RestOfWords]) :-   % stop at reserved words, verbs or prepositions. 
    %(member(Word, StopWords); reserved_word(Word); verb(Word); preposition(Word); punctuation(Word); phrase(newline, [Word])), !.  % or punctuation
    (member(Word, StopWords); that_(Word); list_symbol(Word); punctuation(Word); phrase(newline, [Word])), !.
extract_variable(SW, InName, OutName, InType, OutType, [' '|RestOfWords], NextWords) :- !, % skipping spaces
    extract_variable(SW, InName, OutName, InType, OutType, RestOfWords, NextWords).
extract_variable(SW, InName, OutName, InType, OutType, ['\t'|RestOfWords], NextWords) :- !, % skipping spaces
    extract_variable(SW, InName, OutName, InType, OutType, RestOfWords, NextWords).  
extract_variable(SW, InName, OutName, InType, OutType, [Word|RestOfWords], NextWords) :- % ordinals are not part of the type
    ordinal(Word), !, 
    extract_variable(SW, [Word|InName], OutName, InType, OutType, RestOfWords, NextWords).
extract_variable(SW, InName, OutName, InType, OutType, [Word|RestOfWords], NextWords) :- % types are not part of the name
    is_a_type(Word),
    extract_variable(SW, InName, NextName, InType, OutType, RestOfWords, NextWords),
    (NextName = [] -> OutName = [Word]; OutName = NextName), !.
extract_variable(SW, InName, [Word|OutName], InType, [Word|OutType], [Word|RestOfWords], NextWords) :- % everything else is part of the name (for instances) and the type (for templates)
    extract_variable(SW, InName, OutName, InType, OutType, RestOfWords, NextWords).

% extract_expression/4
% extract_expression(+StopWords, ListOfNameWords, +ListOfWords, NextWordsInText)
% it does not stop at reserved words!
extract_expression(_, [], [], []) :- !.                                % stop at when words run out
extract_expression(StopWords, [], [Word|RestOfWords], [Word|RestOfWords]) :-   % stop at  verbs? or prepositions?. 
    (member(Word, StopWords); that_(Word); list_symbol(Word); parenthesis(Word), phrase(newline, [Word])), !.  
%extract_expression([Word|RestName], [Word|RestOfWords], NextWords) :- % ordinals are not part of the name
%    ordinal(Word), !,
%    extract_constant(RestName, RestOfWords, NextWords).
extract_expression(SW, RestName, [' '|RestOfWords],  NextWords) :- !, % skipping spaces
    extract_expression(SW, RestName, RestOfWords, NextWords).
extract_expression(SW, RestName, ['\t'|RestOfWords],  NextWords) :- !, 
    extract_expression(SW, RestName, RestOfWords, NextWords).
extract_expression(SW, [Word|RestName], [Word|RestOfWords],  NextWords) :-
    %is_a_type(Word),
    %not(determiner(Word)), % no determiners inside constants!
    extract_expression(SW, RestName, RestOfWords, NextWords).

% extract_constant/4
% extract_constant(+StopWords, ListOfNameWords, +ListOfWords, NextWordsInText)
extract_constant(_, [], [], []) :- !.                                % stop at when words run out
extract_constant(StopWords, [], [Word|RestOfWords], [Word|RestOfWords]) :-   % stop at reserved words, verbs? or prepositions?. 
    %(member(Word, StopWords); reserved_word(Word); verb(Word); preposition(Word); punctuation(Word); phrase(newline, [Word])), !.  % or punctuation
    (member(Word, StopWords); that_(Word); list_symbol(Word); parenthesis(Word); punctuation(Word); phrase(newline, [Word])), !.
%extract_constant([Word|RestName], [Word|RestOfWords], NextWords) :- % ordinals are not part of the name
%    ordinal(Word), !,
%    extract_constant(RestName, RestOfWords, NextWords).
extract_constant(SW, RestName, [' '|RestOfWords],  NextWords) :- !, % skipping spaces
    extract_constant(SW, RestName, RestOfWords, NextWords).
extract_constant(SW, RestName, ['\t'|RestOfWords],  NextWords) :- !, 
    extract_constant(SW, RestName, RestOfWords, NextWords).
extract_constant(SW, [Word|RestName], [Word|RestOfWords],  NextWords) :-
    %is_a_type(Word),
    %not(determiner(Word)), % no determiners inside constants!
    extract_constant(SW, RestName, RestOfWords, NextWords).

%extract_string/3
extract_string([], [], []) :- !.
extract_string([], [newline(A)|RestOfWords], [newline(A)|RestOfWords]):- !.
extract_string([String], InWords, NextWords) :-
    extract_all_string([newline(_)], Words, InWords, NextWords),
    concat_atom(Words, '', String). 

extract_all_string(StopWords, [], [Word|RestOfWords], RestOfWords) :-
    member(Word, StopWords), !. 
extract_all_string(StopWords, [Word|RestString], [Word|RestOfWords], NextWords) :-
    extract_all_string(StopWords, RestString, RestOfWords, NextWords ). 

% extract_list/6
% extract_list(+StopWords, -List, +Map1, -Map2, +[Word|PossibleLiteral], -NextWords),
extract_list(SW, [], Map, Map, [Word|Rest], [Word|Rest]) :- 
    lists:member(Word, SW), !. % stop but leave the symbol for further verification
%extract_list(_, [], Map, Map, [')'|Rest], [')'|Rest]) :- !. 
extract_list(SW, RestList, Map1, MapN, [' '|RestOfWords],  NextWords) :- !, % skipping spaces
    extract_list(SW, RestList, Map1, MapN, RestOfWords, NextWords).
extract_list(SW, RestList, Map1, MapN, [' '|RestOfWords],  NextWords) :- !, % skipping spaces
    extract_list(SW, RestList, Map1, MapN, RestOfWords, NextWords).
extract_list(SW, RestList, Map1, MapN, ['\t'|RestOfWords],  NextWords) :- !, 
    extract_list(SW, RestList, Map1, MapN, RestOfWords, NextWords).
extract_list(SW, RestList, Map1, MapN, [','|RestOfWords],  NextWords) :- !, % skip over commas
    extract_list(SW, RestList, Map1, MapN, RestOfWords, NextWords).
extract_list(SW, RestList, Map1, MapN, ['|'|RestOfWords],  NextWords) :- !, % skip over |
    extract_list(SW, RestList, Map1, MapN, RestOfWords, NextWords).
extract_list(StopWords, List, Map1, MapN, [Det|InWords], LeftWords) :-
    phrase(indef_determiner, [Det|InWords], RInWords), 
    extract_variable(['|'|StopWords], [], NameWords, [], _, RInWords, NextWords), NameWords \= [], % <- leave that _ unbound!
    name_predicate(NameWords, Name),  
    update_map(Var, Name, Map1, Map2),
    (NextWords = [']'|_] -> (RestList = [], LeftWords=NextWords, MapN=Map2 ) ; 
    extract_list(StopWords, RestList, Map2, MapN, NextWords, LeftWords) ), 
    (RestList\=[] -> List=[Var|RestList]; List=[Var]), 
    !.
extract_list(StopWords, List, Map1, MapN, [Det|InWords], LeftWords) :-
    phrase(def_determiner, [Det|InWords], RInWords), 
    extract_variable(['|'|StopWords], [], NameWords, [], _, RInWords, NextWords), NameWords \= [], % <- leave that _ unbound!
    name_predicate(NameWords, Name),  
    consult_map(Var, Name, Map1, Map2), 
    (NextWords = [']'|_] -> (RestList = [], LeftWords=NextWords, MapN=Map2 ) ;
    extract_list(StopWords, RestList, Map2, MapN, NextWords, LeftWords) ), 
    (RestList\=[] -> List=[Var|RestList]; List=[Var]), !.
extract_list(StopWords, List, Map1, MapN, InWords, LeftWords) :- % symbolic variables without determiner
    extract_variable(['|'|StopWords], [], NameWords, [], _, InWords, NextWords), NameWords \= [],  % <- leave that _ unbound!
    name_predicate(NameWords, Name),  
    consult_map(Var, Name, Map1, Map2), 
    (NextWords = [']'|_] -> (RestList = [], LeftWords=NextWords, MapN=Map2 ) ; 
    extract_list(StopWords, RestList, Map2, MapN, NextWords, LeftWords) ), 
    (RestList\=[] -> List=[Var|RestList]; List=[Var]), !.
extract_list(StopWords, List, Map1, MapN, InWords, LeftWords) :-
    extract_expression(['|',','|StopWords], NameWords, InWords, NextWords), NameWords \= [], 
    ( phrase(expression_(Expression, Map1, Map2), NameWords) ->   true 
    ; ( Map1 = Map2, name_predicate(NameWords, Expression) ) ),
    ( NextWords = [']'|_] -> ( RestList = [], LeftWords=NextWords, MapN=Map2 ) 
    ;    extract_list(StopWords, RestList, Map2, MapN, NextWords, LeftWords) ), 
    extend_list(RestList, Expression, List), !. %  print_message(informational, " ~q "-[List]), !. 
    %(RestList=[_,_|_] -> List=[Expression|RestList] ; 
    %    RestList = [One] -> List=[Expression, One] ;
    %        RestList = [] -> List = [[]] ), !.

extend_list([A,B|R], X, List) :- append([X], [A,B|R], List). 
extend_list([A], X, [X|[A]]).
extend_list([], X, [X]). 

determiner --> ind_det, !.
determiner --> ind_det_C, !.
determiner --> def_det, !.
determinar --> def_det_C. 

indef_determiner --> ind_det, !.
indef_determiner --> ind_det_C. 

def_determiner --> def_det, !.
def_determiner --> def_det_C. 

rebuild_template(RawTemplate, Map1, MapN, Template) :-
    template_elements(RawTemplate, Map1, MapN, [], Template).

% template_elements(+Input,+InMap, -OutMap, +Previous, -Template)
template_elements([], Map1, Map1, _, []).     
template_elements([Word|RestOfWords], Map1, MapN, Previous, [Var|RestTemplate]) :-
    (phrase(ind_det, [Word|RestOfWords], RRestOfWords); phrase(ind_det_C,[Word|RestOfWords], RRestOfWords)), Previous \= [is|_], 
    extract_variable([], [], NameWords, [], _, RRestOfWords, NextWords), !, % <-- CUT!
    name_predicate(NameWords, Name), 
    update_map(Var, Name, Map1, Map2), 
    template_elements(NextWords, Map2, MapN, [], RestTemplate).
template_elements([Word|RestOfWords], Map1, MapN, Previous, [Var|RestTemplate]) :-
    (phrase(def_det, [Word|RestOfWords], RRestOfWords); phrase(def_det_C,[Word|RestOfWords], RRestOfWords)), Previous \= [is|_], 
    extract_variable([], [], NameWords, [], _, RRestOfWords, NextWords), !, % <-- CUT!
    name_predicate(NameWords, Name), 
    member(map(Var,Name), Map1),  % confirming it is an existing variable and unifying
    template_elements(NextWords, Map1, MapN, [], RestTemplate).
template_elements([Word|RestOfWords], Map1, MapN, Previous, [Word|RestTemplate]) :-
    template_elements(RestOfWords, Map1, MapN, [Word|Previous], RestTemplate).

% update_map/4
% update_map(?V, +Name, +InMap, -OutMap)
update_map(V, Name, InMap, InMap) :- 
    var(V), nonvar(Name), nonvar(InMap), 
    member(map(O,Name), InMap), O\==V, fail, !. 
update_map(V, Name, InMap, OutMap) :-  % updates the map by adding a new variable into it. 
    var(V), nonvar(Name), nonvar(InMap), 
    not(member(map(_,Name), InMap)), 
    OutMap = [map(V,Name)|InMap]. 
%update_map(V, _, Map, Map) :-
%    nonvar(V). 

% consult_map/4
% consult_map(+V, -Name, +Inmap, -OutMap)
consult_map(V, Name, InMap, InMap) :-
    member(map(Var, SomeName), InMap), (Name == SomeName -> Var = V; ( Var == V -> Name = SomeName ; fail ) ),  !.  
%consult_map(V, V, Map, Map). % leave the name unassigned % deprecated to be used inside match

builtin_(BuiltIn, [BuiltIn1, BuiltIn2|RestWords], RestWords) :- 
    atom_concat(BuiltIn1, BuiltIn2, BuiltIn), 
    Predicate =.. [BuiltIn, _, _],  % only binaries fttb
    predicate_property(system:Predicate, built_in), !.
builtin_(BuiltIn, [BuiltIn|RestWords], RestWords) :- 
    Predicate =.. [BuiltIn, _, _],  % only binaries fttb
    predicate_property(system:Predicate, built_in). 

/* --------------------------------------------------------- Utils in Prolog */
time_of(P, T) :- P=..[_|Arguments], lists:append(_, [T], Arguments). % it assumes time as the last argument

% Unwraps tokens, excelt for newlines which become newline(NextLineNumber)
unpack_tokens([], []).
unpack_tokens([cntrl(Char)|Rest], [newline(Next)|NewRest]) :- (Char=='\n' ; Char=='\r'), !,
    %not sure what will happens on env that use \n\r
    update_nl_count(Next), unpack_tokens(Rest, NewRest).
unpack_tokens([First|Rest], [New|NewRest]) :-
    (First = word(New); First=cntrl(New); First=punct(New); First=space(New); First=number(New); First=string(New)), 
     !,
    unpack_tokens(Rest, NewRest).  

% increments the next line number
update_nl_count(NN) :- retract(last_nl_parsed(N)), !, NN is N + 1, assert(last_nl_parsed(NN)). 

ordinal(Ord) :-
    ordinal(_, Ord). 

ordinal(1,  'first').
ordinal(2,  'second').
ordinal(3,  'third').
ordinal(4,  'fourth').
ordinal(5,  'fifth').
ordinal(6,  'sixth').
ordinal(7,  'seventh').
ordinal(8,  'eighth').
ordinal(9,  'ninth').
ordinal(10, 'tenth').
% french
ordinal(1, 'premier').
ordinal(2, 'seconde').
ordinal(3, 'troisième').
ordinal(4, 'quatrième').
ordinal(5, 'cinquième').
ordinal(6, 'sixième').
ordinal(7, 'septième').
ordinal(8, 'huitième').
ordinal(9, 'neuvième').
ordinal(10, 'dixième'). 
% spanish male
ordinal(1, 'primero').
ordinal(2, 'segundo').
ordinal(3, 'tercero').
ordinal(4, 'cuarto').
ordinal(5, 'quinto').
ordinal(6, 'sexto').
ordinal(7, 'séptimo').
ordinal(8, 'octavo').
ordinal(9, 'noveno').
ordinal(10, 'decimo'). 
% spanish female
ordinal(1, 'primera').
ordinal(2, 'segunda').
ordinal(3, 'tercera').
ordinal(4, 'cuarta').
ordinal(5, 'quinta').
ordinal(6, 'sexta').
ordinal(7, 'séptima').
ordinal(8, 'octava').
ordinal(9, 'novena').
ordinal(10, 'decima'). 

%is_a_type/1
is_a_type(T) :- % pending integration with wei2nlen:is_a_type/1
   %ground(T),
   (is_type(T); pre_is_type(T)), !. 
   %(T=time; T=date; T=number; T=person; T=day). % primitive types to start with
   %not(number(T)), not(punctuation(T)),
   %not(reserved_word(T)),
   %not(verb(T)),
   %not(preposition(T)). 

/* ------------------------------------------------ determiners */

ind_det_C --> ['A'].
ind_det_C --> ['An'].
ind_det_C --> ['Un'].     % spanish, italian, and french
ind_det_C --> ['Una'].    % spanish, italian
ind_det_C --> ['Une'].    % french
ind_det_C --> ['Qui'].    % french which? 
ind_det_C --> ['Quoi'].    % french which? 
ind_det_C --> ['Uno'].    % italian
ind_det_C --> ['Che']. % italian which
ind_det_C --> ['Quale']. % italian which
% ind_det_C('Some').
ind_det_C --> ['Each'].   % added experimental
ind_det_C --> ['Which'].  % added experimentally
ind_det_C --> ['Cuál'].   % added experimentally spanish

def_det_C --> ['The'].
def_det_C --> ['El'].  % spanish
def_det_C --> ['La'].  % spanish, italian, and french
def_det_C --> ['Le'].  % french
def_det_C --> ['L'], [A], {atom_string(A, "'")}.   % french
def_det_C --> ['Il'].  % italian
def_det_C --> ['Lo'].  % italian

ind_det --> [a].
ind_det --> [an].
ind_det --> [another]. % added experimentally
ind_det --> [which].   % added experimentally
ind_det --> [each].    % added experimentally
ind_det --> [un].      % spanish, italian, and french
ind_det --> [una].     % spanish, italian
ind_det --> [une].     % french
ind_det --> [qui].     % french which?
ind_det --> [quel].    % french which? masculine    
ind_det --> [quelle].  % french which? femenine
ind_det --> [che]. % italian which
ind_det --> [quale]. % italian which
ind_det --> [uno].     % italian
ind_det --> ['cuál'].    % spanish
% ind_det(some).

def_det --> [the].
def_det --> [el].     % spanish
def_det --> [la].     % spanish, italian and french
def_det --> [le].     % french
def_det --> [l], [A], {atom_string(A, "'")}.  % french, italian
def_det --> [il].     % italian
def_det --> [lo].     % italian

/* ------------------------------------------------ reserved words */
reserved_word(W) :- % more reserved words pending??
    W = 'is'; W ='not'; W='if'; W='If'; W='then'; W = 'where';  W = '&'; % <- hack!
    W = 'at'; W= 'from'; W='to';  W='half'; % W='or'; W='and'; % leaving and/or out of this for now
    W = 'else'; W = 'otherwise'; 
    W = such ; 
    W = '<'; W = '='; W = '>';  W = '+'; W = '-'; W = '/'; W = '*'; % these are handled by extract_expression
    W = '{' ; W = '}' ; W = '(' ; W = ')' ; W = '[' ; W = ']',
    W = ':', W = ','; W = ';'. % these must be handled by parsing
reserved_word(P) :- punctuation(P).

that_(that).
that_('That'). 

/* ------------------------------------------------ punctuation */
%punctuation(punct(_P)).

punctuation('.').
punctuation(',').
punctuation(';').
%punctuation(':').
punctuation('\'').

/* ------------------------------------------------ verbs */
verb(Verb) :- present_tense_verb(Verb); continuous_tense_verb(Verb); past_tense_verb(Verb). 

present_tense_verb(is).
present_tense_verb(complies). 
present_tense_verb(does). 
present_tense_verb(occurs).
present_tense_verb(meets).
present_tense_verb(relates).
present_tense_verb(can).
present_tense_verb(qualifies).
present_tense_verb(has).
present_tense_verb(satisfies).
present_tense_verb(owns).
present_tense_verb(belongs).
present_tense_verb(applies).
present_tense_verb(must).
present_tense_verb(acts).
present_tense_verb(falls).
present_tense_verb(corresponds). 
present_tense_verb(likes). 

continuous_tense_verb(according).
continuous_tense_verb(beginning).
continuous_tense_verb(ending).

past_tense_verb(spent). 
past_tense_verb(looked).
past_tense_verb(could).
past_tense_verb(had).
past_tense_verb(tried).
past_tense_verb(explained).
past_tense_verb(ocurred).
 
/* ------------------------------------------------- prepositions */
preposition(of).
%preposition(on).
preposition(from).
preposition(to).
preposition(at).
preposition(in).
preposition(with).
preposition(plus).
preposition(as).
preposition(by).

/* ------------------------------------------------- memory handling */
assertall([]).
assertall([F|R]) :-
    not(asserted(F)),
    %print_message(informational, "Asserting ~w"-[F]),
    assertz(F), !,
    assertall(R).
assertall([_F|R]) :-
    assertall(R).

asserted(F :- B) :- clause(F, B). % as a rule with a body
asserted(F) :- clause(F,true). % as a fact

/* -------------------------------------------------- error handling */
currentLine(LineNumber, Rest, Rest) :-
    once( nth1(_,Rest,newline(NextLine)) ), LineNumber is NextLine-2. 

% assert_error_os/1
% to save final error to be displayed
assert_error_os([]) :- !. 
assert_error_os([error(Message, LineNumber, Tokens)|Re]) :-
    asserta(error_notice(error, Message, LineNumber, Tokens)),
    assert_error_os(Re).

asserterror(Me, Rest) :-
    %print_message(error, ' Error found'), 
    %select_first_section(Rest, 40, Context), 
    %retractall(error_notice(_,_,_,_)), % we will report only the last
    once( nth1(N,Rest,newline(NextLine)) ), LineNumber is NextLine-2,
    RelevantN is N-1,
    length(Relevant,RelevantN), append(Relevant,_,Rest),
    findall(Token, (member(T,Relevant), (T=newline(_) -> Token='\n' ; Token=T)), Tokens),
    asserta(error_notice(error, Me, LineNumber, Tokens)). % asserting the last first!

% to select just a chunck of Rest to show. 
select_first_section([], _, []) :- !.
select_first_section(_, 0, []) :- !. 
select_first_section([E|R], N, [E|NR]) :-
    N > 0, NN is N - 1,
    select_first_section(R, NN, NR). 

showErrors(File,Baseline) :- % showing the deepest message!
    findall(error_notice(error, Me,Pos, ContextTokens), 
        error_notice(error, Me,Pos, ContextTokens), ErrorsList),
    deepest(ErrorsList, 
        error_notice(error, 'None',0, ['There was no syntax error']), 
        error_notice(error, MeMax,PosMax, ContextTokensMax)), 
    atomic_list_concat([MeMax,': '|ContextTokensMax],ContextTokens_),
    Line is PosMax+Baseline,
    print_message(error,error(syntax_error(ContextTokens_),file(File,Line,_One,_Char))).
    % to show them all
    %forall(error_notice(error, Me,Pos, ContextTokens), (
    %    atomic_list_concat([Me,': '|ContextTokens],ContextTokens_),
    %    Line is Pos+Baseline,
    %    print_message(error,error(syntax_error(ContextTokens_),file(File,Line,_One,_Char)))
    %    )).

deepest([], Deepest, Deepest) :- !.
deepest([error_notice(error, Me,Pos, ContextTokens)|Rest], 
        error_notice(error,_Me0, Pos0,_ContextTokens0), Out) :-
    Pos0 < Pos, !, 
    deepest(Rest, error_notice(error, Me,Pos, ContextTokens), Out).
deepest([_|Rest], In, Out) :-
    deepest(Rest, In, Out).

showProgress :-
    findall(error_notice(error, Me,Pos, ContextTokens), 
        error_notice(error, Me,Pos, ContextTokens), ErrorsList),
    deepest(ErrorsList, 
        error_notice(error, 'None',0, ['There was no syntax error']), 
        error_notice(error, MeMax,PosMax, ContextTokensMax)), 
    atomic_list_concat([MeMax,': '|ContextTokensMax],ContextTokens_),
    Line is PosMax+1,
    print_message(informational,error(syntax_error(ContextTokens_),file(someFile,Line,_One,_Char))).


spypoint(A,A). % for debugging

% meta_dictionary(?LiteralElements, ?NamesAndTypes, ?Template)
% for meta templates. See below
% meta_dictionary/1
meta_dictionary(Predicate, VariablesNames, Template) :- 
    meta_dict(Predicate, VariablesNames, Template) ; predef_meta_dict(Predicate, VariablesNames, Template).

:- discontiguous predef_meta_dict/3.
predef_meta_dict([\=, T1, T2], [first_thing-time, second_thing-time], [T1, is, different, from, T2]).
predef_meta_dict([=, T1, T2], [first_thing-time, second_thing-time], [T1, is, equal, to, T2]).

% dictionary(?LiteralElements, ?NamesAndTypes, ?Template)
% this is a multimodal predicate used to associate a Template with its particular other of the words for LE
% with the Prolog expression of that relation in LiteralElements (not yet a predicate, =.. is done elsewhere).
% NamesAndTypes contains the external name and type (name-type) of each variable just in the other in 
% which the variables appear in LiteralElement. 
% dictionary/1
dictionary(Predicate, VariablesNames, Template) :- % dict(Predicate, VariablesNames, Template).
    dict(Predicate, VariablesNames, Template) ; predef_dict(Predicate, VariablesNames, Template).
%    predef_dict(Predicate, VariablesNames, Template); dict(Predicate, VariablesNames, Template).

:- discontiguous predef_dict/3.
% predef_dict/3 is a database with predefined templates for LE
% it must be ordered by the side of the third argument, to allow the system to check first the longer template
% with the corresponding starting words. 
% for Taxlog examples
predef_dict(['\'s_R&D_expense_credit_is', Project, ExtraDeduction, TaxCredit], 
                                 [project-projectid, extra-amount, credit-amount],
   [Project, '\'s', 'R&D', expense, credit, is, TaxCredit, plus, ExtraDeduction]).
predef_dict(['can_request_R&D_relief_such_as', Project, ExtraDeduction, TaxCredit], 
                                 [project-projectid, extra-amount, credit-amount],
   [Project, can, request,'R&D', relief, for, a, credit, of, TaxCredit, with, a, deduction, of, ExtraDeduction]).
predef_dict(['\'s_sme_R&D_relief_is', Project, ExtraDeduction, TaxCredit], 
                                 [project-projectid, extra-amount, credit-amount],
   [the, 'SME', 'R&D', relief, for, Project, is, estimated, at, TaxCredit, with, an, extra, of, ExtraDeduction]).
predef_dict([project_subject_experts_list_is,Project,Experts], [project-object, experts_list-list],
   [Project, has, an, Experts, list]).
predef_dict([rollover_applies,EventID,Asset,Time,Transferor,TransfereesList], [id-event,asset-asset,when-time,from-person,to-list], 
   [EventID, rollover, of, the, transfer, of, Asset, from, Transferor, to, TransfereesList, at, Time, applies]).
predef_dict([transfer_event,ID,Asset,Time,Transferor,TransfereesList],[id-id,asset-asset,time-time,from-person,to-list],
   [event, ID, of, transfering, Asset, from, Transferor, to, TransfereesList, at, Time, occurs]).
predef_dict([s_type_and_liability_are(Asset,Type,Liability), [asset-asset, assettype-type, liabilty-amount],
   [the, type, of, asset, Asset, is, Type, its, liability, is, Liability]]).
predef_dict([exempt_transfer,From,To,SecurityIdentifier,Time],[from-taxpayer,to-taxpayer,secID-number, time-time],
   [a, transfer, from, From, to, To, with, SecurityIdentifier, at, Time, is, exempt]).
predef_dict([shares_transfer,Sender,Recipient,SecurityID,Time], [from-person, to-person, id-number, time-time], 
   [Sender, transfers, shares, to, Recipient, at, Time, with, id, SecurityID]).
predef_dict([trading_in_market,SecurityID,MarketID,Time], [id-number,market-number,time-time], 
   [whoever, is, identified,by, SecurityID, is, trading, in, market, MarketID, at, Time]).
predef_dict([uk_tax_year_for_date,Date,Year,Start,End], [date-date,year-year,start-date,end-date], 
   [date, Date, falls, in, the, 'UK', tax, year, Year, that, starts, at, Start, ends, at, End]).
predef_dict([days_spent_in_uk,Individual,Start,End,TotalDays], [who-person,start-date,end-date,total-number], 
   [Individual, spent, TotalDays, days, in, the, 'UK', starting, at, Start, ending, at, End]).
predef_dict([days_spent_in_uk,Individual,Start,End,TotalDays], [who-person,start-date,end-date,total-number], 
                   [Individual, spent, TotalDays, in, the, 'UK', starting, at, Start, &, ending, at, End]). 
predef_dict([uk_tax_year_for_date,Date,Year,Start,End], [first_date-date, year-year, second_date-date, third_date-date], 
                   [in, the, 'UK', Date, falls, in, Year, beginning, at, Start, &, ending, at, End]).
predef_dict([is_individual_or_company_on, A, B],
                   [affiliate-affiliate, date-date],
                   [A, is, an, individual, or, is, a, company, at, B]).
% Prolog
predef_dict([has_as_head_before, A, B, C], [list-list, symbol-term, rest_of_list-list], [A, has, B, as, head, before, C]).
predef_dict([append, A, B, C],[first_list-list, second_list-list, third_list-list], [appending, A, then, B, gives, C]).
predef_dict([reverse, A, B], [list-list, other_list-list], [A, is, the, reverse, of, B]).
predef_dict([same_date, T1, T2], [time_1-time, time_2-time], [T1, is, the, same, date, as, T2]). % see reasoner.pl before/2
predef_dict([between,Minimum,Maximum,Middle], [min-date, max-date, middle-date], 
                [Middle, is, between, Minimum, &, Maximum]).
predef_dict([is_1_day_after, A, B], [date-date, second_date-date],
                [A, is, '1', day, after, B]).
predef_dict([is_days_after, A, B, C], [date-date, number-number, second_date-date],
                  [A, is, B, days, after, C]).
predef_dict([immediately_before, T1, T2], [time_1-time, time_2-time], [T1, is, immediately, before, T2]). % see reasoner.pl before/2
predef_dict([\=, T1, T2], [thing_1-thing, thing_2-thing], [T1, is, different, from, T2]).
predef_dict([==, T1, T2], [thing_1-thing, thing_2-thing], [T1, is, equivalent, to, T2]).
predef_dict([is_a, Object, Type], [object-object, type-type], [Object, is, of, type, Type]).
predef_dict([is_not_before, T1, T2], [time1-time, time2-time], [T1, is, not, before, T2]). % see reasoner.pl before/2
predef_dict([=, T1, T2], [thing_1-thing, thing_2-thing], [T1, is, equal, to, T2]).
predef_dict([isbefore, T1, T2], [time1-time, time2-time], [T1, is, before, T2]). % see reasoner.pl before/2
predef_dict([isafter, T1, T2], [time1-time, time2-time], [T1, is, after, T2]).  % see reasoner.pl before/2
predef_dict([member, Member, List], [member-object, list-list], [Member, is, in, List]).
predef_dict([is_, A, B], [term-term, expression-expression], [A, is, B]). % builtin Prolog assignment
% predefined entries:
%predef_dict([assert,Information], [info-clause], [this, information, Information, ' has', been, recorded]).
predef_dict([\=@=, T1, T2], [thing_1-thing, thing_2-thing], [T1, \,=,@,=, T2]).
predef_dict([\==, T1, T2], [thing_1-thing, thing_2-thing], [T1, \,=,=, T2]).
predef_dict([=\=, T1, T2], [thing_1-thing, thing_2-thing], [T1, =,\,=, T2]).
predef_dict([=@=, T1, T2], [thing_1-thing, thing_2-thing], [T1, =,@,=, T2]).
predef_dict([==, T1, T2], [thing_1-thing, thing_2-thing], [T1, =,=, T2]).
predef_dict([=<, T1, T2], [thing_1-thing, thing_2-thing], [T1, =,<, T2]).
predef_dict([=<, T1, T2], [thing_1-thing, thing_2-thing], [T1, =,<, T2]).
predef_dict([>=, T1, T2], [thing_1-thing, thing_2-thing], [T1, >,=, T2]).
predef_dict([=, T1, T2], [thing_1-thing, thing_2-thing], [T1, =, T2]).
predef_dict([<, T1, T2], [thing_1-thing, thing_2-thing], [T1, <, T2]).
predef_dict([>, T1, T2], [thing_1-thing, thing_2-thing], [T1, >, T2]).
predef_dict([unparse_time, Secs, Date], [secs-time, date-date], [Secs, corresponds, to, date, Date]).
predef_dict([must_be, Type, Term], [type-type, term-term], [Term, must, be, Type]).
predef_dict([must_not_be, A, B], [term-term, variable-variable], [A, must, not, be, B]). 

% pre_is_type/1
pre_is_type(thing).
pre_is_type(time).
pre_is_type(type).
pre_is_type(object).
pre_is_type(date).
pre_is_type(day).
pre_is_type(person).
pre_is_type(list). 
pre_is_type(number). 

% support predicates
must_be(A, var) :- var(A).
must_be(A, nonvar) :- nonvar(A).
must_be_nonvar(A) :- nonvar(A).
must_not_be(A,B) :- not(must_be(A,B)). 

has_as_head_before([B|C], B, C). 

% see reasoner.pl
%before(A,B) :- nonvar(A), nonvar(B), number(A), number(B), A < B. 

matches_name(Word, [Element|_], [Name-_|_], Name) :- Word == Element, !.
matches_name(Word, [_|RestElem], [_|RestTypes], Name) :-
    matches_name(Word, RestElem, RestTypes, Name). 

matches_type(Word, [Element|_], [_-Type|_], Type) :- Word == Element, !.
matches_type(Word, [_|RestElem], [_|RestTypes], Type) :-
    matches_type(Word, RestElem, RestTypes, Type). 

delete_underscore([], []) :- !. 
delete_underscore(['_'|Rest], Final) :- delete_underscore(Rest, Final), !.  
delete_underscore([W|Rest], [W|Final]) :- delete_underscore(Rest, Final).     

add_determiner([Word|RestWords], [Det, Word|RestWords]) :-
    name(Word,[First|_]), proper_det(First, Det).

proper_det(97, an) :- !.
proper_det(101, an) :- !.
proper_det(105, an) :- !.
proper_det(111, an) :- !.
proper_det(117, an) :- !.
proper_det(_, a). 

% ---------------------------------------------------------------- sandbox

sandbox:safe_primitive(le_input:source_lang(_)).
sandbox:safe_primitive(le_input:is_type(_)).
sandbox:safe_primitive(le_input:dict(_,_,_)).
sandbox:safe_primitive(le_input:meta_dict(_,_,_)).
sandbox:safe_primitive(le_input:assertall(_)).
sandbox:safe_primitive(le_input:asserted(_)). 