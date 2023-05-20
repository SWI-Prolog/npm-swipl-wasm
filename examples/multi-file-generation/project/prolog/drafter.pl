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
    draft_string/2, draft_string_from_file/2, test_draft/2,
    nameToWords/2,
    predicateWords/3, printAllPredicateWords/1, uniquePredicateWords/2, uniqueArgSentences/2, uniquePredicateSentences/2]).

:- use_module('spacy/spacy.pl').
:- use_module(kp_loader).

% Knowledge page drafting aids, assuming loaded content and using Spacy parses and known knowledge pages (modules)

%! test_draft(+Text,-DraftedCode)
%  Draft some source code from a given text string
test_draft(Text,DraftedCode) :-
    load_content( [_{url:testURL,text:Text}] ),
    draft_string(testURL,DraftedCode).

%  Draft some source code from a given text file
draft_string_from_file(File,DraftedCode) :-
    load_content_from_text_file(File,URL),
    draft_string(URL,DraftedCode).

%  Args is (for now..) a list of role names
%  Why includes relevant sentences and tokens within the TextURL's text, Spacy extraction
:- thread_local predicate_draft/4. % TextURL,Functor,Args,Why 

draft_string(URL,S) :- 
    draft(URL,Tmp), read_file_to_string(Tmp,S,[]).


% draft(+URL,-TmpPrologFile)
draft(URL,TmpFile):-
    must_be(atomic,URL), must_be(var,TmpFile),
    retractall(predicate_draft(URL,_,_,_)),
    refreshTokens(URL),
    % Now that we've (re) parsed tokens, detect we shall:
    forall((
        content_tokens_in(URL,SpecificURL,SI,Tokens,Extraction),
        detected_predicate(Tokens,Functor,Args,Reason)
        ),
        %TODO: detect duplicates/variants
        assert(predicate_draft(URL,Functor,Args,Extraction/SpecificURL/SI/Reason)
    )),
    % Now generate the Prolog code:
    tmp_file_stream(TmpFile, S, [encoding(text),extension(pl)]),
    format(S,":- module('~a',[]).~n~n",[URL]),
    forall(predicate_draft(URL,Functor,Args_,Why),(
        Why=_/SpecificURL/SI/_, 
        content_tokens(SpecificURL, SI, Tokens,_), sentence(Tokens,Sentence),
        maplist(capitalize,Args_,Args),
        Pred=..[Functor|Args],
        format(S,"% ~w.~n%  Why: ~w~n%  To parse sentence:~n% parseAndSee('~a',SentenceI,Tokens,Tree).~n~n",[Pred,Why,Sentence])
        )),
    close(S).

% detected_predicate(+Tokens,-Functor,-Args,-Reason)
% See predicates and notes on tags etc. in spacy.pl 
detected_predicate(Tokens,F,Args,VerbToken) :- 
    member_with([lemma=L_,tag=VerbTag,pos=verb,i=Vi_], VerbToken, Tokens), 
    VerbTag\=md, % must not be a modal auxiliary
    (L_=="be" -> (
        member_with([head=Vi_,dep=acomp,lemma=RealL,i=Vi],Tokens), 
        atomic_list_concat([L_,'_',RealL],LL), atom_string(L,LL)
        ) ; (
            L=L_, Vi=Vi_
        )
    ), 
    atom_string(F,L),
    findall(Arg,(
        (
            member_with([head=Vi,dep=nsubj],Tokens), Arg_="Subject" 
            ; member_with([head=Vi,dep=dobj],Tokens), Arg_="Object"
            ; member_with([head=Vi,dep=prep,lemma=Arg_],Tokens)),
        atom_string(Arg,Arg_)
        ),Args).

capitalize(X,NewX) :- 
    name(X,[First|Codes]), to_upper(First,U), name(NewX,[U|Codes]).

%! nameToWords(PrologAtom,Words) is det
%  Breaks a predicate or variable name into words, if detected via underscores or spaces or CamelCase
nameToWords(V,['ANONVAR']) :- var(V), !.
nameToWords('',[]) :- !.
nameToWords([X1|Xn],Words) :- !, 
    nameToWords(X1,W1), nameToWords(Xn,Wn), append(W1,Wn,Words).
nameToWords([],[]) :- !.
nameToWords(X,[Word]) :- \+ atomic(X), !, term_string(X,Word).
nameToWords(X,Words) :- atomics_to_string(Words_,'_',X), Words_=[_,_|_], !, nameToWords(Words_,Words).
nameToWords(X,Words) :- atomics_to_string(Words_,' ',X), Words_=[_,_|_], !, nameToWords(Words_,Words).
nameToWords(X,Words) :- camelsToList(X,Words_), Words_=[_,_|_], !, nameToWords(Words_,Words).
nameToWords(X,[X]).

camelsToList(X,L) :- 
    must_be(atomic,X), assertion(X\==''), atom_codes(X,Codes), 
    %(code_type(C,upper)->Type=upper;code_type(C,lower)->Type=lower;Type=other),
    camelsToList(Codes,white,[],L).

% camelsToList(CharCodes,LastType,NextWordCharsSoFar,Words)
camelsToList([C|Codes],LastType,NextCodes,NewWords) :- 
    %Changers=[upper,digit], member(Changer,Changers), code_type(C,Changer), Type\=Changer, 
    code_type(C,Type), 
    LastType\=Type,
    member(LastType-Type,[lower-upper,upper-digit,lower-digit,digit-lower,digit-upper]),
    !,
    (NextCodes=[]->NewWords=Words ; NewWords=[W|Words]),
    atom_codes(W,NextCodes), camelsToList([C|Codes],Type,[],Words).
camelsToList([C|Codes],LastType,NextCodes,Words) :- !,
    (code_type(C,LastType)->Type=LastType;
        code_type(C,lower)->Type=lower;
        code_type(C,upper)->Type=upper;
        code_type(C,digit)->Type=digit;
        once(code_type(C,Type))),
    append(NextCodes,[C],NewNextCodes),
    camelsToList(Codes,Type,NewNextCodes,Words).
camelsToList([],_,NextCodes,Words) :- 
    (NextCodes=[] -> Words=[] ; (atom_codes(W,NextCodes), Words=[W])).

%! predicateWords(?KP,?Pred,-FunctorWords,-WordArgsList)
%  Pred is a predicate literal template
% E.g. all_kps_loaded, predicateWords(KP,Pred,PredsWords), member(F/N/Fwords/Awords,PredsWords), atomics_to_string(Fwords,' ',Fstring),  format("~w:  ~a~n",[F/N,Fstring]), forall(member(A,Awords),(atomics_to_string(A,' ',Astring),format("  ~a~n",[Astring]))), fail.
predicateWords(KP,Pred,PredsWords) :-
    all_kps_loaded(KP),
    setof(F/Arity/Fwords/ArgsWords, How^Args^( 
        kp_predicate_mention(KP,Pred,How), 
        functor(Pred,F,Arity), nameToWords(F,Fwords),
        predicate_literal(KP,Pred), Pred=..[F|Args],
        findall(ArgWords, (member(Arg,Args), nameToWords(Arg,ArgWords)), ArgsWords)
    ),PredsWords).

printAllPredicateWords(KP) :-
    predicateWords(KP,_Pred,PredsWords), 
    member(F/N/Fwords/Awords,PredsWords), atomics_to_string(Fwords,' ',Fstring),  
    format("~w:  ~a~n",[F/N,Fstring]), 
    forall(member(A,Awords),(atomics_to_string(A,' ',Astring),format("  ~a~n",[Astring]))), 
    fail.
printAllPredicateWords(_).

uniquePredicateSentences(KP,Sentences) :-
    setof(PredsWords,Pred^predicateWords(KP,Pred,PredsWords),L),
    append(L,All),
    setof(Fwords, F^Arity^ArgsWords^member(F/Arity/Fwords/ArgsWords,All), Sentences).

uniqueArgSentences(KP,Sentences) :-
    setof(PredsWords,Pred^predicateWords(KP,Pred,PredsWords),L),
    append(L,All),
    setof(ArgsWords, F^Arity^Fwords^member(F/Arity/Fwords/ArgsWords,All), Sentences).

% ignores one letter words
% e.g. ?- forall(uniquePredicateWords(KP,Words), format("~w~n  ~w~n",[KP,Words])).
uniquePredicateWords(KP,Words) :-
    setof(PredsWords,Pred^predicateWords(KP,Pred,PredsWords),L),
    append(L,All),
    setof(Word, F^Arity^Fwords^ArgsWords^ArgWords^C1^C2^Rest^(
        member(F/Arity/Fwords/ArgsWords,All), 
        (member(Word,Fwords); member(ArgWords,ArgsWords), member(Word,ArgWords)),
        atom_codes(Word,[C1,C2|Rest])
        ), Words).


%TODO: handle more verb patterns, e.g. have+dobj, etc.
%TODO: generate rules, extract nouns/concepts/class hierarchies, knowledge page/reference extractor

:- multifile sandbox:safe_primitive/1.
sandbox:safe_primitive(drafter:predicateWords(_,_,_)).
