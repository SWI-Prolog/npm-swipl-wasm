:- module(_,[
    loaded_kp/1, all_kps_loaded/0, all_kps_loaded/1, kp_dir/1, taxkb_dir/1, kp_location/3, kp/1, must_succeed/2, must_succeed/1,
    shouldMapModule/2, module_api_hack/1, moduleMapping/2, myDeclaredModule/1, system_predicate/1,
    discover_kps_in_dir/1, discover_kps_in_dir/0, discover_kps_gitty/0, setup_kp_modules/0, load_kps/0,
    load_gitty_files/1, load_gitty_files/0, save_gitty_files/1, save_gitty_files/0, delete_gitty_file/1, update_gitty_file/3,
    xref_all/0, xref_clean/0, print_kp_predicates/0, print_kp_predicates/1, reset_errors/0, my_xref_defined/3, url_simple/2,
    kp_predicate_mention/3, predicate_literal/2,load_named_file/3, 
    edit_kp/1, swish_editor_path/2, knowledgePagesGraph/1, knowledgePagesGraph/2]).

:- use_module(library(prolog_xref)).
:- use_module(library(broadcast)).

:- multifile prolog:message//1.

:- dynamic kp_dir/1, taxkb_dir/1.
:- prolog_load_context(directory, D), 
    retractall(taxkb_dir(_)), assert(taxkb_dir(D)), 
    retractall(kp_dir(_)), atomic_list_concat([D,'/kb'], KD), assert(kp_dir(KD)), 
    print_message(informational,"KB directory is ~a"-[KD]).

/** <module> Dynamic module loader.

Scans a given set of Prolog files in SWISH storage or in a file system directpry, and identifies "knowledge pages", files which:
- are modules named with an URL
Can also export and import SWISH storage to/from a file system directory.
*/

:- dynamic kp_location/4. % URL,File,ModifiedTime,InGitty
kp_location(URL,File,InGitty) :- kp_location(URL,File,_,InGitty).

kp(URL_) :- 
    (nonvar(URL_) -> atom_string(URL,URL_);URL=URL_),
    kp_location(URL,_,_).

%! discover_kps_in_dir(+Dir) is det.
%
discover_kps_in_dir(Dir) :-
    retractall(kp_location(_,_,_,false)),
    forall(directory_member(Dir,File,[extensions([pl])]), (
        time_file(File,Modified),
        open(File,read,In),
        process_file(In,File,Modified,false)
    )).

% This also RELOADS modules already loaded
discover_kps_in_dir :-
    kp_dir(D), discover_kps_in_dir(D).

process_file(In,File,Modified,InGitty) :-
    must_be(boolean,InGitty),
    setup_call_cleanup( true, (
        process_terms(In, LastTerm),
        % (LastTerm=at(Name) -> (
        (LastTerm=(:-module(Name,_)) -> (
            ((kp_location(Name,PreviousFile,PreviousMod,InGitty), PreviousMod>=Modified) -> 
                print_message(warning,ignored_older_module(Name,PreviousFile,File)) ; 
                (
                    (kp_location(Name,PreviousFile,_,InGitty) -> 
                        print_message(warning,using_newer_module(Name,PreviousFile,File)) 
                        ; true),
                    retractall(kp_location(Name,_,_,InGitty)),
                    assert(kp_location(Name,File,Modified,InGitty)),
                    % reload the module if it already exists:
                    (current_module(Name) -> load_named_file(File,Name,InGitty) ; true)
                ))
            ); true)
    ), close(In)).

prolog:message(ignored_older_module(Module,PreviousFile,File)) --> 
    ['Ignored older file ~a for module ~w; sticking to ~a'-[File,Module,PreviousFile]].
prolog:message(using_newer_module(Module,PreviousFile,File)) --> 
    ['Forgot older file ~a for module ~w; using instead ~a'-[PreviousFile,Module,File]].

process_terms(In,Term) :- % actually gets only the first term, where the module declaration must be:
    %repeat, 
    read_term(In, Term, [syntax_errors(fail)]),
    ( Term==end_of_file, ! ; 
        Term= (:- module(URL,_)), is_absolute_url(URL), ! ; 
        true
        %Term=at(Name), (ground(Name)->true; print_message(warning,'ignored'(at(Name))), fail) 
    ).


declare_our_metas(Module) :-
    Module:meta_predicate(mainGoal(0,+)),
    Module:meta_predicate(on(0,?)),
    Module:meta_predicate(because(0,-)).

% load_named_file(+File,+Module,+InGittyStorage)
load_named_file(File,Module,InGittyStorage) :-
    load_named_file_(File,Module,InGittyStorage),
    kp_file_modified(Module,Modified,InGittyStorage),
    retractall(kp_location(Module,File,_,InGittyStorage)),
    assert(kp_location(Module,File,Modified,InGittyStorage)),
    (xref_source(Module,[silent(true)]) -> true ; print_message(warning,"failed xref_source"-[])).

load_named_file_(File,Module,true) :- !,
    %print_message(informational, "load File into Module ~w ~w\n"-[File, Module]), 
    use_gitty_file(Module:File,[/* useless: module(Module)*/]).
load_named_file_(File,Module,false) :- 
    load_files(File,[module(Module)]).

load_kps :- 
    forall(kp_location(URL,File,InGitty), (
        load_named_file(File,URL,InGitty)
    )).

setup_kp_modules :- forall(kp(M), setup_kp_module(M) ).

setup_kp_module(M) :-
    M:discontiguous((if)/2),
    M:discontiguous((on)/2),
    M:discontiguous((because)/2),
    M:discontiguous(question/2), M:discontiguous(question/3),
    declare_our_metas(M).

all_kps_loaded :- all_kps_loaded(_).

all_kps_loaded(KP):-
    print_message(informational,"Loading Knowledge Page(s)..(~w)"-[KP]),
    forall(kp(KP),loaded_kp(KP)).

:- thread_local module_api_hack/1.

%! loaded_kp(++KnowledgePageName) is nondet.
%
%  loads the knowledge page, failing if it cannot
loaded_kp(Name) :- module_api_hack(Name), !.
loaded_kp(Name) :- must_be(nonvar,Name), shouldMapModule(_,Name), !. % SWISH module already loaded 
loaded_kp(Name) :- \+ kp_location(Name,_,_), !, 
    (\+ reported_missing_kp(Name) -> (
        assert(reported_missing_kp(Name)), print_message(error,"Unknown knowledge page: ~w"-[Name])) 
        ; true), 
    fail.
loaded_kp(Name) :- % some version already loaded:
    module_property(Name,last_modified_generation(T)), T>0, 
    !,
    once(( kp_file_modified(Name,FT,InGitty), kp_location(Name,File,LastModified,InGitty) )), 
    (FT>LastModified -> (
        load_named_file(File,Name,InGitty), 
        print_message(informational,"Reloaded ~w"-[Name])
        ) ; true).
loaded_kp(Name) :- kp_location(Name,File,InGitty), !, % first load:
    load_named_file(File,Name,InGitty),
    (\+ reported_loaded_kp(Name) -> (
        print_message(informational,loaded(Name,File)), assert(reported_loaded_kp(Name))) 
        ; true).
loaded_kp(Name) :- 
    \+ reported_missing_kp(Name), 
    print_message(error,no_kp(Name)), 
    assert(reported_missing_kp(Name)), fail.

kp_file_modified(Name,Time,InGitty) :- 
    kp_location(Name,File,InGitty),
    (InGitty==true -> (storage_meta_data(File, Meta), Time=Meta.time) ; time_file(File,Time)).


:-thread_local reported_missing_kp/1.
:-thread_local reported_loaded_kp/1.

reset_errors :- 
    retractall(reported_missing_kp(_)), retractall(reported_loaded_kp(_)).

prolog:message(loaded(Module,Path)) --> ['Loaded ~w from ~a'-[Module,Path]].


% Support xref for gitty and file system files
:- multifile
	prolog:xref_source_identifier/2,
	prolog:xref_open_source/2,
    prolog:xref_close_source/2,
    prolog:xref_source_time/2,
    prolog:meta_goal/2.

prolog:xref_source_identifier(URL, URL) :- kp_location(URL,_,_).

prolog:xref_open_source(URL, Stream) :-
    kp_location(URL,File,InGitty),
    (InGitty==true -> (storage_file(File,Data,_Meta), open_string(Data, Stream))
        ; (open(File,read,Stream))).

prolog:xref_close_source(_, Stream) :-
	close(Stream).

prolog:xref_source_time(URL, Modified) :-
    kp_location(URL,_File,Modified,_InGitty).


%! xref_all is det
%
% refresh xref database for all knowledge pages %TODO: report syntax errors properly
xref_all :- 
    forall(kp_location(URL,File,_), (
        print_message(informational,xreferencing(URL,File)), 
        xref_source(URL,[silent(true)]) % to avoid spurious warnings for mainGoal singleton vars
    )).

prolog:message(xreferencing(URL,File)) --> ['Xreferencing module ~w in file ~w'-[URL,File]].
prolog:message(no_kp(Name)) --> ["Could not find knowledge page ~w"-[Name]].

xref_clean :-
    forall(kp_location(URL,_,_), xref_clean(URL)).


% kp_predicate_mention(?Module,?PredicateTemplate,?How) How is called_by(KP)/defined
% Considers undefined predicates too; ignores mentions from example scenarios
kp_predicate_mention(KP,G,How) :-
    (nonvar(KP) -> true ; kp(KP)),
    ( xref_defined(KP,G,_), How=defined ; 
      xref_called(KP, Called, _By), (Called=_:G->true;Called=G), How=called_by(KP)
      ),
    \+ prolog:meta_goal(G,_), \+ system_predicate(G).

%! predicate_argnames(+KP,?PredicateTemplate) is nondet.
%  Grounds argument variables with their source names AS MUCH AS POSSIBLE, using system meta information from the clauses mentioning the predicate
%  KP must be already loaded. Anonymous variables are not ground.
predicate_literal(M,Pred) :- must_be(nonvar,M),
    (M:clause(Pred,Body,Ref) ; my_xref_called(M,Pred,By), clause(M:By,Body,Ref), \+ \+ contains_term(Pred,Body)), 
    clause_info(Ref,_,_,_,[variable_names(Names)]),
    bind_vars_with_names(Pred:-Body,Names).
%TODO: should use a contains_term with variant/2 instead

%! bind_vars_with_names(?Term,+VarNames)
% VarNames is a list of Name=Var
bind_vars_with_names(T,VN) :- bind_vars_with_names(T,VN,_).

bind_vars_with_names(_,[],[]) :- !.
bind_vars_with_names(V,[Name=Var|VN],NewVN) :- var(V), !, 
    (var(Var) -> (Var=V,Name=Var,NewVN=VN) ; (bind_vars_with_names(V,VN,NewVN))).
bind_vars_with_names(X,VN,VN) :- atomic(X), !.
bind_vars_with_names([X1|Xn],VN1,VNn) :- !, bind_vars_with_names(X1,VN1,VN2), bind_vars_with_names(Xn,VN2,VNn).
bind_vars_with_names(X,VN1,VNn) :- compound_name_arguments(X,_,Args), bind_vars_with_names(Args,VN1,VNn).

print_kp_predicates :- print_kp_predicates(_).

% This also LOADS the modules, to access the examples:
print_kp_predicates(KP) :- %TODO: ignore subtrees of because/2
    all_kps_loaded,
    forall(kp(KP),(
        format("---~nKP: ~w~n",[KP]),
        format("  Examples:~n"),
        forall(catch(KP:example(Name,Scenarios),_,fail),(
            aggregate(sum(N),( member(scenario(Facts,_Assertion),Scenarios), length(Facts,N)), Total),
            format("    ~w: ~w facts~n",[Name,Total])
            )),
        format("  Instance data:~n"),
        forall(xref_defined(KP,G,thread_local(_)), (
            functor(G,F,N), format("    ~w~n",[F/N])
            )),
        format("  Defined predicates:~n"),
        forall((xref_defined(KP,G,How),How\=thread_local(_)), (
            functor(G,F,N), format("    ~w~n",[F/N])
        )),
        format("  External predicates called:~n"),
        forall((
            xref_called(KP, Called, _By),
            Called=Other:G, Other\=KP,
            (\+ prolog:meta_goal(G,_))
            ), 
            (functor(G,F,N), format("    ~w (~w)~n",[F/N,Other]))
        ),
        format("  UNDEFINED predicates:~n"),
        forall((
            xref_called(KP, Called, _), 
            (Called=Other:G -> Other\=KP ; (Called=G,Other=KP)),
            (\+ prolog:meta_goal(G,_)),
            \+ my_xref_defined(Other,G,_),
            \+ system_predicate(G)
            ), 
            (functor(G,F,N), format("    ~w (~w)~n",[F/N,Other]))
        )

    )). 

% check that the source has already been xref'ed, otherwise xref would try to load it and cause an "iri_scheme" error:
my_xref_defined(M,G,Class) :- 
    xref_current_source(M), xref_defined(M,G,Class).
my_xref_called(M,Pred,By) :-
    xref_current_source(M), xref_called(M,Pred,By).

system_predicate(G) :- predicate_property(G,built_in). 
system_predicate(G) :- kp_dir(D), predicate_property(G,file(F)), \+ sub_atom(F,_,_,_,D).
system_predicate(example(_,_)).
system_predicate(mainGoal(_,_)).
system_predicate(query(_,_)).
system_predicate(question(_,_)).
system_predicate(question(_,_,_)).
system_predicate(irrelevant_explanation(_)).
system_predicate(function(_,_)).

url_simple(URL,Simple) :- \+ sub_atom(URL,_,_,_,'/'), !, 
    Simple=URL.
url_simple(URL,Simple) :- 
    parse_url(URL,L), memberchk(path(P),L), atomics_to_string(LL,'/',P), 
    ((last(LL,Simple),Simple\='') -> true ;
        LL = [Simple] -> true;
        append(_,[Simple,_],LL)),
    !.
url_simple(URL,URL).
    
:- meta_predicate(must_succeed(0,+)).
must_succeed(G,_) :- G, !.
must_succeed(G,M) :- throw("weird_failure_of of ~w: ~w"-[G,M]).

must_succeed(G) :- must_succeed(G,'').

:- thread_local myDeclaredModule_/1. % remembers the module declared in the last SWISH window loaded
% filters the SWISH declared module with known KPs; the term_expansion hack catches a lot of other modules too, such as 'http_stream'
myDeclaredModule(M) :- myDeclaredModule_(M), kp(M), !.

swish_editor_path(KP,Path) :- must_be(nonvar,KP),
    (kp_location(KP,File,true)->true;File=not_on_swish_storage),
    format(string(Path),"/p/~a",[File]), !.


:- if(current_module(swish)). %%% only when running with the SWISH web server:
:- use_module(swish(lib/storage)).
:- use_module(swish(lib/gitty)).
:- use_module(library(pengines)).

%! discover_kps_gitty is det.
%
%  Scans all Prolog files in SWISH's gitty storage for knowledge pages. RELOADS
%  already loaded modules, but does not delete "orphans" (modules no longer in gitty)
%  TODO: use '$destroy_module'(M) on those?
discover_kps_gitty :-
    retractall(kp_location(_,_,_,true)),
    forall(storage_file_extension(File,pl),(
        storage_file(File,Data,Meta),
        open_string(Data, In),
        process_file(In,File,Meta.time,true)
    )).

%! save_gitty_files(+ToDirectory) is det
%
%  ERASES the directory and copies all gitty Prolog files into it
%  MAKE SURE ToDirectory has source versioning control!
save_gitty_files(_ToDirectory) :- \+ storage_file_extension(_File,pl), !, 
    print_message(warning,"No gitty files to save"-[]).
save_gitty_files(ToDirectory) :-
    (exists_directory(ToDirectory)->true; make_directory(ToDirectory)),
    delete_directory_contents(ToDirectory),
    forall(storage_file_extension(File,pl),(
        storage_file(File,Data,Meta),
        directory_file_path(ToDirectory,File,Path),
        open(Path,write,S), write_term(S,Data,[]), close(S),
        set_time_file(Path, _OldTimes, [modified(Meta.time)])
        )).

save_gitty_files :- 
    kp_dir(D), save_gitty_files(D).

%! load_gitty_files(+FromDirectory) is det
%
%  Updates or creates (in gitty storage) all Prolog files from the given file system directory; sub-directories are ignored.
%  Does not delete the other (pre-existing) gitty files
% Example: load_gitty_files('/Users/mc/git/TaxKB/kb').
load_gitty_files(From) :- 
    forall(directory_member(From,Path,[extensions([pl])]),(
        read_file_to_string(Path,Data,[]),
        time_file(Path,Modified),
        directory_file_path(_,File,Path),
        update_gitty_file(File,Modified,From,Data)
    )).

load_gitty_files :-
    kp_dir(D), load_gitty_files(D).

% update_gitty_file(+Filename,+ModifiedTime,+Origin,+Text)
update_gitty_file(File,Modified,Origin,Data) :-
    web_storage:open_gittystore(Store),
    current_user(User,_Email),
    (gitty_file(Store, File, OldHead) -> (
        storage_meta_data(File, Meta), 
        NewMeta = Meta.put([previous=OldHead, modify=[any, login, owner], (public)=true, time=Modified, author=User]),
        gitty_update(Store, File, Data, NewMeta, _CommitRet)
        ) ; (
        gitty_create(Store, File, Data, _{update_gitty_file:Origin, modify:[any, login, owner], public:true, time:Modified, author:User }, _CommitRet)
        )
    ).

update_gitty_file(File,Origin,Data) :- 
    get_time(Now), update_gitty_file(File,Now,Origin,Data).

%! delete_gitty_file(+GittyFile) is det
%
% makes the file empty, NOT a proper delete
delete_gitty_file(File) :-
    must_be(nonvar,File),
    web_storage:open_gittystore(Store),
    gitty_file(Store, File, OldHead),
    % I was unable to effectively delete:
    % gitty:delete_head(Store, OldHead), gitty:delete_object(Store, OldHead), % this is only effective after a SWISH restart
    % broadcast(swish(deleted(File, OldHead))). % not doing anything, possibly missing something on the JS end
    % ... instead this does roughly what the DELETE REST SWISH endpoint in storage.pl does:
    storage_meta_data(File, Meta),
    NewMeta = Meta.put([previous=OldHead]),
    gitty_update(Store, File, "", NewMeta, _CommitRet).

:- listen(swish(X),reactToSaved(X)). % note: do NOT use writes!, they would interfere with SWISH's internal REST API
/*
reactToSaved(created(GittyFile,Commit)) :- % discover and xref
    storage_file(GittyFile,Data,Meta), process_file(Data,GittyFile,Meta.time,true), 
    reactToSaved(updated(GittyFile,Commit)).
reactToSaved(updated(GittyFile,_Commit)) :- % xref
    kp_location(URL,GittyFile,true), 
    xref_source(URL,[silent(true)]).
*/

reactToSaved(created(GittyFile,Commit)) :-
    reactToSaved(updated(GittyFile,Commit)).
reactToSaved(updated(GittyFile,_Commit)) :- % discover (module name may have changed...) and xref
    %mylog(updated(GittyFile,_Commit)),
    storage_file(GittyFile,Data,Meta), 
    open_string(Data,In),
    must_succeed(process_file(In,GittyFile,Meta.time,true)), 
    (kp_location(URL,GittyFile,true) -> xref_source(URL,[silent(true)]) ; 
        print_message(warning,"Could not find URL for ~w"-[GittyFile])).

%! edit_kp(URL) is det
%
% Open the current gitty version of the knowledge page in SWISH's editor
edit_kp(KP) :-
    kp_location(KP,_File,InGitty),
    (InGitty==(false) -> print_message(error,"~w is not in SWISH storage"-[KP]);(
        swish_editor_path(KP,Path),
        format(string(URL),"http://localhost:3050~a",[Path]), www_open_url(URL)
        )).

%%%% Knowledge pages graph

:- multifile user:'swish renderer'/2. % to avoid SWISH warnings in other files
:- use_rendering(user:graphviz).

knowledgePagesGraph(KP,dot(digraph([rankdir='LR'|Graph]))) :- 
    % xref_defined(KP, Goal, ?How)
    setof(edge(From->To,[]), KP^Called^By^ByF^ByN^OtherKP^G^CalledF^CalledN^How^(
        kp(KP), xref_called(KP, Called, By),
        functor(By,ByF,ByN), From = at(ByF/ByN,KP),
        (Called=OtherKP:G -> true ; ( once(xref_defined(KP,Called,How)), OtherKP=KP, G=Called)),
        \+ prolog:meta_goal(G,_),
        functor(G,CalledF,CalledN), To = at(CalledF/CalledN,OtherKP) 
        %term_string(From_,From,[quoted(false)]), term_string(To_,To,[quoted(false)]), url_simple(ArcRole_,ArcRole)
        ),Edges), 
    setof(node(ID,[/*shape=Shape*/label=Label]), KP^Goal^How^GF^GN^From^EA^Pred^Abrev^(
        (
            kp(KP), xref_defined(KP, Goal, How),
            functor(Goal,GF,GN),
            ID = at(GF/GN,KP)
            ;
            member(edge(From->ID,EA),Edges) % calls to undefined predicates
        ),
        ID=at(Pred,KP), url_simple(KP,Abrev),
        format(string(Label),"~w at ~w",[Pred,Abrev])
        %(hypercube(R,ID) -> Shape=box3d ; Shape=ellipse)
        ), Nodes),
    append(Nodes,Edges,Items),
    Graph=Items.
    %(var(SizeInches) -> Graph=Items ; Graph = [size=SizeInches|Items]).

knowledgePagesGraph(G) :- knowledgePagesGraph(_,G).

:- multifile sandbox:safe_primitive/1.
sandbox:safe_primitive(kp_loader:knowledgePagesGraph(_,_)).
sandbox:safe_primitive(kp_loader:print_kp_predicates(_)).
sandbox:safe_primitive(kp_loader:load_gitty_files). %TODO: this should be restricted to power users
sandbox:safe_primitive(kp_loader:save_gitty_files).
sandbox:safe_primitive(kp_loader:all_kps_loaded).
sandbox:safe_primitive(web_storage:open_gittystore(_)).
sandbox:safe_primitive(gitty:gitty_file(_, _, _)).
sandbox:safe_primitive(gitty:load_commit(_,_,_)). 
sandbox:safe_primitive(gitty:gitty_update(_, _, _, _, _)). 
sandbox:safe_primitive(gitty:size_in_bytes(_,_)). 
sandbox:safe_primitive(gitty:save_object(_,_,_,_)).
sandbox:safe_primitive(gitty:gitty_create(_,_,_,_,_)).

%%%% assist editor navigation; cf. swish/web/js/codemirror/mode/prolog/prolog_server.js

:- use_module(library(http/http_json)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).

:- http_handler(codemirror(xref),   token_references,        []).
token_references(Request) :-
    %http_read_json_dict(Request, Query, [value_string_as(atom)]),
    http_parameters(Request, [arity(Arity,[integer]),text(Text,[]),type(Type,[]),file(Module,[optional(true)]),uuid(UUID,[optional(true)])]),
    % UUID is the SWISH internal module for our current editor's text
    % mylog(gotQuery/Type/Text/Arity/Module/UUID),
    % asserta(my_request(Query)), % for debugging
    (nonvar(UUID) -> (xref_module(UUID,MyModule), Ignorable=[UUID,MyModule]); Ignorable=[]),
    catch(term_string(Term_,Text),_,fail), 
    functor(Term_,Functor,_),
    (atom(Term_) -> functor(Term,Functor,Arity); Term=Term_), % hack to fix longclicks on body goals
    (sub_atom(Type, 0, _, _, head) -> ( % a clause head
        must_be(var,Module),
        findall( _{title:Title,line:Line,file:File,target:Functor}, ( % regex built on the Javascript side from target
            xref_called(OtherModule,_Mine:Term,By,_Cond,Line), functor(By,F,N), format(string(Title),"A call from ~w",[F/N]),
            \+ member(OtherModule,Ignorable),
            kp_location(OtherModule,File,_InGitty) 
            ),Locations)
        ) ; 
        sub_atom(Type, 0, _, _, goal) -> ( % a goal in a clause body
            findall( _{title:Title,line:Line,file:File,target:Functor}, ( 
            xref_defined(Module,Term,How), arg(1,How,Line), format(string(Title),"A definition for ~a",[Text]),
            kp_location(Module,File,_InGitty) 
            ),Locations)
        ) ; 
        throw(weird_token_type(Type))
    ),
    %Solution = _{hello: "Good Afternoon!", functor:Functor, arity:Arity, module:File},
    reply_json_dict(Locations).

% This at the end, as it activates the term expansion (no harm done otherwise, just some performance..):
user:term_expansion((:-module(M,L)),(:-module(M,L))) :- !, assert(myDeclaredModule_(M)). 
:- multifile pengines:prepare_module/3.
:- thread_local myCurrentModule/1. % the new temporary SWISH module where our query runs
pengines:prepare_module(Module, swish, _Options) :- 
    % this seems to hold always, but commenting it out just in case...: assertion( \+ myCurrentModule(_)),
    setup_kp_module(Module),
    assert(myCurrentModule(Module)).
    % should we perhaps use this_capsule...??
% there is (just arrived from the SWISH editor) a fresher version To of the declared module From
% ...OR there WAS,  although it no longer exists
shouldMapModule(From,To) :- myDeclaredModule(From), kp(From), myCurrentModule(To), !, 
    (moduleMapping(From,To)->true;(assert(moduleMapping(From,To)))).

:- dynamic moduleMapping/2. % Nice module->transient SWISH module; remembers previous mappings, to support UI navigation later, e.g. from explanations


current_user(User,Email) :- 
    pengine_user(U), get_dict(user,U,User), Email=U.user_info.email, 
    !.
current_user(unknown_user,unknown_email).

:- else. % vanilla SWI-Prolog

current_user(unknown_user,unknown_email).

shouldMapModule(_,_) :- fail.
moduleMapping(_,_) :- fail.

%! edit_kp(URL) is det
%
% Open the filed version of the knowledge page in the user editor
edit_kp(URL) :-
    kp_location(URL,File,InGitty),
    (InGitty==(true) -> print_message(error,"That is in SWISH storage, not in the file system!");(
        edit(file(File))
        )).

discover_kps_gitty :- print_message(informational,'this only works on SWISH'-[]).
load_gitty_files :- throw('this only works on SWISH ').
load_gitty_files(_) :- throw('this only works on SWISH ').
save_gitty_files(_) :- throw('this only works on SWISH ').
save_gitty_files :- throw('this only works on SWISH ').
delete_gitty_file(_) :- throw('this only works on SWISH ').
update_gitty_file(_,_,_) :- throw('this only works on SWISH ').

knowledgePagesGraph(_,_) :- throw('this only works on SWISH').
knowledgePagesGraph(_) :- throw('this only works on SWISH').
gitty_file(_,_,_) :- throw('this only works in SWISH gitty'). 
gitty_update(_, _, _, _, _) :- throw('this only works in SWISH gitty'). 
:- endif.