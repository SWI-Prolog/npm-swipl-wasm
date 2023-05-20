% Copyright Miguel Calejo, 2019-2020; open source, licensed with 3-clause BSD

:- module(_,[
    load_content/1, load_content_from_text_file/2, content/2, refreshTokens/1, content_tokens/4, content_tokens_in/5,
    t_word/2, t_lemma/2, t_pos/2, t_tag/2, t_head/2, t_dep/2, t_i/2, t_offset/2, t_absorbed/2, member_with/3, member_with/2, root/2,
    sentence/2, parseAndSee/4, parseAndSee/5, spaCyParseTokens/3,
    depgraph/2, hierplane/2]).
% Spacy interface: parsing, representation of text chunks and sentences with tokens, utility predicates
% Text and tokens are kept associated with URLs/paths, with an implicit textual hierarchy

%! load_content(+ContentFileOrDicts)  
%  loads content from a file or an ItemsArray list; each item is a _{url:U,text:T} dict
load_content(File) :- atomic(File), !,
    setup_call_cleanup(open(File,read,S),(
        repeat,
        (
            read(S,Term), 
            (Term==end_of_file -> true ; 
                Term=content(URL,Text) -> (update_content(URL,Text), fail) ; 
                (print_message(warning, "ignored content: ~w"-[Term]), fail)
            ))
    ),close(S)), !.
load_content(Items) :- 
    must_be(list,Items), 
    forall(member(Item,Items), update_content(Item.url,Item.text) ).

load_content_from_text_file(File,URLbase) :-
    format(string(URLbase),"file://~a",[File]),
    open(File,read,S),
    create_counter(LC),
    repeat, 
    inc_counter(LC,LN), 
    read_line_to_string(S,Line), 
    (
        Line==end_of_file, !, close(S) 
        ; 
        Line \= "",
        \+ sub_atom(Line,0,_,_,'#'), % not a comment
        format(string(URL),"~a#~w",[URLbase,LN]), 
        update_content(URL,Line), 
        fail
    ).


% content(URL,Text)
% all the text chunks to process, each with a unique URL denoting its origin
content(Path,Text) :- 
    user_can_see_content, content_(Path,Text).

:- thread_local content_/2. % URL, text string

update_content(URL,Text) :- 
    must_be(atomic,URL), must_be(atomic,Text),
    retractall(content_(URL,_)), assert(content_(URL,Text)).

% parsed sentence tokens for each text chunk
content_tokens(URI, Sentence_i, Tokens,Extraction) :- 
    user_can_see_content, content_tokens_(URI, Sentence_i, Tokens,Extraction).

% content_tokens_in(+PrefixURL, ?SpecificURL, ?Sentence_i, ?Tokens, ?Extraction)
content_tokens_in(PrefixURL,URL,SI,Tokens,Extraction) :-
    user_can_see_content, must_be(nonvar,PrefixURL),
    content_tokens_(URL, SI, Tokens,Extraction),
    sub_atom(URL,0,_,_,PrefixURL).

:- thread_local content_tokens_/4. % content_tokens_(URI, Sentence_i, Tokens, Extraction)  

% clear_tokens(+Path,+Extraction)
% clears all tokens for txt chunks prefixed with Path and given extraction
clear_tokens(Prefix,Extraction) :- 
    findall(Path, (content_tokens_(Path, _, _, Extraction), sub_atom(Path,0,_,_,Prefix)), Paths),
    forall(member(Path,Paths),retractall(content_tokens_(Path, _, _, Extraction))).



%TODO: refactor for other parsers, possibly into different module
refreshTokens(Prefix) :-
    Extraction = extraction(spaCyParse,_CollapseNouns,_CollapsePuncts,_When),
    clear_tokens(Path,Extraction),
    get_time(Now), CollapseNouns=1, CollapsePuncts=1,
    NewExtraction = extraction(spaCyParse,CollapseNouns,CollapsePuncts,Now),
    forall((content_(Path, Text), sub_atom(Path,0,_,_,Prefix)),(
        spaCyParse(Text,CollapseNouns,CollapsePuncts,Sentences),
        storeSentences(Sentences,0,0,Path,content_tokens_,NewExtraction)
        )).


%%% Token utilities, graph and tree generators for SWISH rendering

%! parseAndSee(+Text,+CollapseNouns,-SentenceIndex,-Tokens,-Hierplane)
%  Parse English text with spaCy and return a sentence of tokens, including its hierplane tree
parseAndSee(Text,CollapseNouns,SentenceIndex,Tokens,Hierplane) :- 
    spaCyParseTokens(Text,CollapseNouns,0,en,SentenceIndex,Tokens),
    hierplane(Tokens,Hierplane).

parseAndSee(Text,SentenceIndex,Tokens,Hierplane) :-
    parseAndSee(Text,1,SentenceIndex,Tokens,Hierplane).


% t(...); tag and POS and dependency labels are kept as lowercase atoms; the rest as strings
%TODO: replace by dict-based representation for terser queries etc. e.g. member_with(i=T.head,Head,Tokens), Head.pos=verb
/** 
	t_word(?Token,?Word)  is nondet 

	t_word(?Token,?Word)
	Convenience accessor to a token term argument, or "field"
*/
t_word(t(_I,_Offset,Word,_Lemma,_POS,_Tag,_Head,_Dep,_Absorbed),Word).
t_lemma(t(_I,_Offset,_Word,Lemma,_POS,_Tag,_Head,_Dep,_Absorbed),Lemma).
t_pos(t(_I,_Offset,_Word,_Lemma,POS,_Tag,_Head,_Dep,_Absorbed),POS).
t_tag(t(_I,_Offset,_Word,_Lemma,_POS,Tag,_Head,_Dep,_Absorbed),Tag).
t_head(t(_I,_Offset,_Word,_Lemma,_POS,_Tag,Head,_Dep,_Absorbed),Head).
t_dep(t(_I,_Offset,_Word,_Lemma,_POS,_Tag,_Head,Dep,_Absorbed),Dep).
t_i(t(I,_Offset,_Word,_Lemma,_POS,_Tag,_Head,_Dep,_Absorbed),I).
t_offset(t(_I,Offset,_Word,_Lemma,_POS,_Tag,_Head,_Dep,_Absorbed),Offset).
t_absorbed(t(_I,_Offset,_Word,_Lemma,_POS,_Tag,_Head,_Dep,Absorbed),Absorbed). % List of tokens that were "chunked" into this, e.g. to improve presentation
% replace Absorbed slot:
t_absorb(t(I,Offset,Word,Lemma,POS,Tag,Head,Dep,Old), Absorbed, t(I,Offset,Word,Lemma,POS,Tag,Head,Dep,New)) :-
    must_be(list,Old), must_be(list,Absorbed), append(Old,Absorbed,New).

%! member_with(?Conditions,?Token,+Tokens)
%  Tokens has a Token complying to Condition (or a list thereof); each condition is a token_field_name=value, e.g. pos=verb
member_with(i=I,T,Tokens):-!, t_i(T,I), (nonvar(I) -> memberchk(T,Tokens) ; member(T,Tokens)). % i is a key!
member_with(head=Head,T,Tokens):-!, t_head(T,Head), member(T,Tokens).
member_with(pos=POS,T,Tokens):-!, t_pos(T,POS), member(T,Tokens). 
member_with(tag=Tag,T,Tokens):-!, t_tag(T,Tag), member(T,Tokens). 
member_with(dep=Dep,T,Tokens):-!, t_dep(T,Dep), member(T,Tokens). 
member_with(word=Word,T,Tokens):-!, t_word(T,Word), member(T,Tokens). 
member_with(lemma=L,T,Tokens):-!, t_lemma(T,L), member(T,Tokens). 
member_with([C|Conditions],T,Tokens):-!, member_with(C,T,Tokens), member_with(Conditions,T,Tokens).
member_with([],T,Tokens) :- !, member(T,Tokens).
member_with(AV,_,_) :- domain_error(token_attribute=value,AV).

member_with(Conditions,Tokens) :- member_with(Conditions,_,Tokens).

select_with(i=I,T,Tokens,NewTokens):- !, t_i(T,I), select(T,Tokens,NewTokens). 
select_with(AV,_,_,_) :- domain_error(token_attribute=value,AV).

is_root(T) :- t_dep(T,D), D==root.
root(Tokens,Root) :- must_be(list,Tokens), member(Root,Tokens), is_root(Root), !.
is_leaf(T,Tokens) :- t_i(T,I), \+ member_with(head=I,_,Tokens).

% Absorb aux verbs into their head
chunkVerbs(Tokens,ChunkedTokens) :- 
    select_with(i=AuxI,Aux,Tokens,Tokens1), (t_dep(Aux,aux);t_dep(Aux,auxpass)), is_leaf(Aux,Tokens), t_head(Aux,HI), 
    abs(AuxI-HI) =:= 1, % hierplane seems to dislike collapsing disjoint tokens, e.g. buggy:
    %spaCyParseTokens("If at any time an Event of Default with respect to a party (the “Defaulting Party”) has occurred and is then continuing, the other party (the “Non-defaulting Party”) may, by not more than 20 days notice to the Defaulting Party specifying the relevant Event of Default, designate a day not earlier than the day such notice is effective as an Early Termination Date in respect of all outstanding Transactions",0,_Tokens), chunkVerbs(_Tokens,_Chunked), hierplane(_Chunked,HP), print_term(HP,[]).
    !,
    select_with(i=HI,Head,Tokens1,Tokens2), t_absorb(Head,[Aux],NewHead),
    chunkVerbs([NewHead|Tokens2],ChunkedTokens).
chunkVerbs(Tokens,ChunkedTokens) :- % let's keep the token index ordering:
    findall(I-T,member_with(i=I,T,Tokens),Pairs),
    sort(Pairs,Sorted),
    findall(T,member(_-T,Sorted),ChunkedTokens).
    
/** 
	sentence(Tokens,Sentence) is det 

	sentence(+Tokens,-Sentence)
	Return the tokens abstracted into a (string) sentence.
*/
sentence(Tokens,Sentence) :- words(Tokens,Sentence).

% words(+Tokens,-WordsString)  enforces token index ordering
words(Tokens,String) :-
    must_be(list,Tokens),
    findall(I-T_,(member_with(i=I,T,Tokens), t_absorbed(T,Absorbed), member(T_,[T|Absorbed])),Pairs),
    sort(Pairs,Sorted),
    findall(T,(member(_-T,Sorted)),SortedTokens),
    concatTokenWords(SortedTokens,String).

% tokens already expanded (from Absorbed)
concatTokenWords([T],Word) :- !, t_word(T,Word).
concatTokenWords([T1,T2|Tokens],String) :- 
    t_word(T1,W1), t_offset(T1,O1), t_offset(T2,O2), string_length(W1,L1),
    (O2-O1>L1 -> Sep = " " ; Sep = ""), 
    concatTokenWords([T2|Tokens],S), atomics_to_string([W1,Sep,S],String).

orderedTokens(Tokens,Ordered) :-
    must_be(list,Tokens),
    findall(I-T,member_with(i=I,T,Tokens),Pairs),
    sort(Pairs,Sorted),
    findall(T,member(_-T,Sorted),Ordered).

/** 
	depgraph(+Tokens,-Digraph) is det 

	depgraph(+Tokens,-Digraph)
	Given a sentence produces a GraphViz dot directed graph specification, ready to display on SWISH.
*/
depgraph(Tokens,dot(digraph([rankdir='BT'|Edges]))) :-
    findall( edge((From->To),[label=Dep]), (
    	member(T,Tokens), t_head(T,HI), \+ is_root(T), 
    	t_dep(T,Dep), t_word(T,Word), t_i(T,I), member_with(i=HI,Head,Tokens), t_word(Head,HeadWord),
     % Nodes are Index:Word; stringify them so Graphviz does NOT take them as node ports:
     term_string(I:Word,From), term_string(HI:HeadWord,To)
     ), Edges).

hierplane(Tokens,HP) :- 
    LTP = _{nsubj:left, nsubjpass:left, csubj:left, mark:left, dobj:right}, % aux:inside
    hierplane(Tokens,_{linkToPosition:LTP},HP).

% hierplane(+Tokens,+StyleMaps,-RenderableHierplaneTerm)
% StyleMaps is a dict representing optional structures in https://github.com/allenai/hierplane#maps
% For rendering see hierplane_renderer.pl
hierplane(Tokens,StyleMaps,hierplane( HP )) :-
    HP = _{text:S, root:R}.put(StyleMaps),
    sentence(Tokens,S),
    root(Tokens,Root),
    Tokens=[First|_], t_offset(First,Delta),
    hierplaneTree(Root,Tokens,Delta,R).

hierplaneTree(T,Tokens,Delta,Node_) :- 
    t_absorbed(T,Absorbed), 
    t_pos(T,POS), t_tag(T,Tag), t_dep(T,Dep), t_i(T,I), 
    orderedTokens([T|Absorbed],Ordered), 
    words([T],Words), 
    tokenSpans(Ordered,Delta,Spans),
    Node = _{nodeType:POS, word:Words, spans:Spans, link:Dep, attributes: [Tag] },
    findall(ChildT, member_with(head=I,ChildT,Tokens), ChildTokens),
    (ChildTokens = [_|_] -> Node_ = Node.put(children,Nodes) ; Node=Node_ ),
    hierplaneTrees(ChildTokens,Tokens,Delta,Nodes).

hierplaneTrees([C1|Cn],Tokens,Delta,[N1|Nn]) :- !,
    hierplaneTree(C1,Tokens,Delta,N1),  hierplaneTrees(Cn,Tokens,Delta,Nn).
hierplaneTrees([],_,_,[]).

tokenSpans([T1|Tn],Delta,[_{start:Start,end:End}|Spans]) :- !, 
    t_offset(T1,Offset), Start is Offset-Delta,
    t_word(T1,Word), string_length(Word,L), End is Start+L,
    tokenSpans(Tn,Delta,Spans).
tokenSpans([],_,[]).

% tokens_to_trees(+Tokens,-Trees)  Trees will be a list of Dep(HeadToken,Children) trees
% Ex: content_tokens_(Path,SI,_Tokens),tokens_to_trees(_Tokens,_Trees), member(_Tree,_Trees), print_term(_Tree,[]).
tokens_to_trees(Tokens1,[root(Root,Children)|Trees]) :- select(Root,Tokens1,Tokens2), is_root(Root), !,
	tokens_to_children_trees(Tokens2,Root,Children,Tokens3), 
	tokens_to_trees(Tokens3,Trees).
tokens_to_trees([],[]).

tokens_to_children_trees(Tokens1,Head,[Child|Children],Tokens) :- t_i(Head,HI), select(C,Tokens1,Tokens2), t_head(C,HI), !,
	t_dep(C,Dep),
	Child=..[Dep,C,GrandChildren],
	tokens_to_children_trees(Tokens2,C,GrandChildren,Tokens3),
	tokens_to_children_trees(Tokens3,Head,Children,Tokens).
tokens_to_children_trees(Tokens,_,[],Tokens).


%%% For spaCy:
% Meaning of dependency labels: https://v2.spacy.io/api/annotation#dependency-parsing

% tags (finer, not the coarser universal POS): https://v2.spacy.io/api/annotation#pos-tagging
% the following was copied from the above and then:
%   tagToPOS(Tag,POS,M,D), string_lower(Tag,TagL), string_lower(POS,POSL), atom_string(Tag_,TagL), atom_string(POS_,POSL), 
%    format("tagToPOS(~q,~q, ~q,~q).~n",[Tag_,POS_,M,D]), fail.
%
% tagToPOS(FineTag,UniversalPOS,Morphology,Description)
tagToPOS($,sym, '','symbol, currency').
tagToPOS('``',punct, 'PunctType=quot PunctSide=ini','opening quotation mark').
tagToPOS('\'\'',punct, 'PunctType=quot PunctSide=fin','closing quotation mark').
tagToPOS(',',punct, 'PunctType=comm','punctuation mark, comma').
tagToPOS('-lrb-',punct, 'PunctType=brck PunctSide=ini','left round bracket').
tagToPOS('-rrb-',punct, 'PunctType=brck PunctSide=fin','right round bracket').
tagToPOS('.',punct, 'PunctType=peri','punctuation mark, sentence closer').
tagToPOS(:,punct, '','punctuation mark, colon or ellipsis').
tagToPOS(add,x, '',email).
tagToPOS(afx,adj, 'Hyph=yes',affix).
tagToPOS(cc,cconj, 'ConjType=comp','conjunction, coordinating').
tagToPOS(cd,num, 'NumType=card','cardinal number').
tagToPOS(dt,det, '',determiner).
tagToPOS(ex,pron, 'AdvType=ex','existential there').
tagToPOS(fw,x, 'Foreign=yes','foreign word').
tagToPOS(gw,x, '','additional word in multi-word expression').
tagToPOS(hyph,punct, 'PunctType=dash','punctuation mark, hyphen').
tagToPOS(in,adp, '','conjunction, subordinating or preposition').
tagToPOS(jj,adj, 'Degree=pos',adjective).
tagToPOS(jjr,adj, 'Degree=comp','adjective, comparative').
tagToPOS(jjs,adj, 'Degree=sup','adjective, superlative').
tagToPOS(ls,x, 'NumType=ord','list item marker').
tagToPOS(md,verb, 'VerbType=mod','verb, modal auxiliary').
tagToPOS(nfp,punct, '','superfluous punctuation').
tagToPOS(nil,x, '','missing tag').
tagToPOS(nn,noun, 'Number=sing','noun, singular or mass').
tagToPOS(nnp,propn, 'NounType=prop Number=sing','noun, proper singular').
tagToPOS(nnps,propn, 'NounType=prop Number=plur','noun, proper plural').
tagToPOS(nns,noun, 'Number=plur','noun, plural').
tagToPOS(pdt,det, '',predeterminer).
tagToPOS(pos,part, 'Poss=yes','possessive ending').
tagToPOS(prp,pron, 'PronType=prs','pronoun, personal').
tagToPOS('prp$',det, 'PronType=prs Poss=yes','pronoun, possessive').
tagToPOS(rb,adv, 'Degree=pos',adverb).
tagToPOS(rbr,adv, 'Degree=comp','adverb, comparative').
tagToPOS(rbs,adv, 'Degree=sup','adverb, superlative').
tagToPOS(rp,adp, '','adverb, particle').
tagToPOS(sp,space, '',space).
tagToPOS(sym,sym, '',symbol).
tagToPOS(to,part, 'PartType=inf VerbForm=inf','infinitival “to”').
tagToPOS(uh,intj, '',interjection).
tagToPOS(vb,verb, 'VerbForm=inf','verb, base form').
tagToPOS(vbd,verb, 'VerbForm=fin Tense=past','verb, past tense').
tagToPOS(vbg,verb, 'VerbForm=part Tense=pres Aspect=prog','verb, gerund or present participle').
tagToPOS(vbn,verb, 'VerbForm=part Tense=past Aspect=perf','verb, past participle').
tagToPOS(vbp,verb, 'VerbForm=fin Tense=pres','verb, non-3rd person singular present').
tagToPOS(vbz,verb, 'VerbForm=fin Tense=pres Number=sing Person=three','verb, 3rd person singular present').
tagToPOS(wdt,det, '','wh-determiner').
tagToPOS(wp,pron, '','wh-pronoun, personal').
tagToPOS('wp$',det, 'Poss=yes','wh-pronoun, possessive').
tagToPOS(wrb,adv, '','wh-adverb').
tagToPOS(xx,x, '',unknown).
tagToPOS('_sp',space, '','').

% for interactive experimentation
spaCyParseTokens(Text,SentenceI,Tokens) :- spaCyParseTokens(Text,en,SentenceI,Tokens).

spaCyParseTokens(Text,Model,SentenceI,Tokens) :- 
    spaCyParseTokens(Text,1,0,Model,SentenceI,Tokens).

spaCyParseTokens(Text,CollapseNouns,CollapsePuncts,Model,SentenceI,Tokens) :- 
    spaCyParse(Text,CollapseNouns,CollapsePuncts,Model,Dicts),
    spaCyDictsToTokens(Dicts,0,0,Pairs),
    member(SentenceI/Tokens,Pairs).

% spaCyDictsToTokens(Dicts,SentenceIndex,WordIndex,Pairs) Pairs is a list of SI/Tokens

spaCyDictsToTokens([S1|Sn],SI,WI,[SI/Tokens|Pairs]) :- !,
    Words = S1.dep_parse.words,
    Arcs = S1.dep_parse.arcs,
    wordTokens(Words,WI,NewWI,Tokens), bindArcs(Arcs,Tokens),
    ((member_with(head=H,Root,Tokens), var(H)) -> H=root, t_dep(Root,root) ; throw(missing_root(S1))),
    assertion(ground(Tokens)),
    NewSI is SI+1,
    spaCyDictsToTokens(Sn,NewSI,NewWI,Pairs).
spaCyDictsToTokens([],_,_,[]).

% storeSentences(+Sentences,+SentenceIndex,+WordIndex,+Path,+FunctorToStore,+Extraction)
storeSentences(Dicts,SentenceIndex,WordIndex,Path,Functor,Extraction) :-
    spaCyDictsToTokens(Dicts,SentenceIndex,WordIndex,Pairs),
    Fact =.. [Functor,Path, SI, Tokens, Extraction],
    forall(member(SI/Tokens,Pairs), assert(Fact)).

% wordTokens(+Words,+WI,-NewWI,-Tokens)
wordTokens([W|Words],WI,NewWI,[T|Tokens]) :- !,
    string_lower(W.tag,TagL), atom_string(Tag,TagL),
    (tagToPOS(Tag,POS,_,_)->true;Tag=POS, print_message(warning,"bad tag: ~w for ~w "-[Tag,W])), % throw(badTag(W)) is too harsh on non English languages...
    t_word(T,W.text), t_tag(T,Tag), t_pos(T,POS), t_i(T,WI),
    t_lemma(T,W.lemma), t_offset(T,W.offset), t_absorbed(T,[]),
    I is WI+1, 
    wordTokens(Words,I,NewWI,Tokens).
wordTokens([],WI,WI,[]).

% bindArcs(+Arcs,?Tokens)
bindArcs([A|Arcs],Tokens) :- !,
    atom_string(Label,A.label),
    (A.dir == "left" -> member_with(i=A.start,T,Tokens), H=A.end ;
        A.dir == "right" -> member_with(i=A.end,T,Tokens), H=A.start
        ; throw(weirdArc(A))),
    t_head(T,H), t_dep(T,Label),
    bindArcs(Arcs,Tokens).
bindArcs([],_).



:- use_module(library(http/http_client)).
:- use_module(library(http/json)).

%%% spaCy testing, https://github.com/jgontrum/spacy-api-docker ; requires SPACY_HOST environment variable set

%parserURL("http://localhost:8080/dep").
spaCyURL(U) :- getenv('SPACY_HOST',Host), format(string(U),"http://~a/sents_dep",[Host]).

%! spaCyParse(+TextOrWordsList,+CollapseNouns,+CollapsePuncts,-Dict) 
% for REST service in https://github.com/jgontrum/spacy-api-docker
spaCyParse(Text,CollapseNouns,CollapsePuncts,Dict) :-
    spaCyParse(Text,CollapseNouns,CollapsePuncts,en,Dict).

% spaCyParse(TextOrList,CollapseNouns,CollapsePuncts,Model,_) :- mylog(spaCyParse(TextOrList,CollapseNouns,CollapsePuncts,Model,_)), fail.
spaCyParse(TextOrList,CollapseNouns,CollapsePuncts,Model,Dict) :-
    (is_list(TextOrList) -> atomic_list_concat(TextOrList," ",Text); TextOrList=Text),
    user_can_parse,
    assertion(CollapseNouns==1;CollapseNouns==0), 
    assertion(CollapsePuncts==1;CollapsePuncts==0), 
    assertion((member(M,[en,de,es,fr,pt,it,nl]), M==Model)),
    must_be(var,Dict),
    atom_string(Text,Text_),
    replace_complicated_words(Text_,Text__),
    %(Text_\=Text__ -> print_message(informational,Text__);true),
    (spaCyURL(URL) -> true ; (print_message(error,"Missing spaCy URL"), fail)),
    format(atom(Post),'{"text":"~a", "model":"~a", "collapse_phrases": ~w, "collapse_punctuation": ~w }',[Text__,Model,CollapseNouns,CollapsePuncts]),
    catch( (http_post(URL, atom(Post), Result, []), atom_json_dict(Result,Dict,[])), Ex, (print_message(error,"spaCy failed for ~a: ~w"-[Text, Ex]), Dict=[])).


replace_complicated_words(Text,NewText) :- 
    findall(Word/Synonym,complicatedWord(Word,Synonym),Pairs), replace_complicated_words(Pairs,Text,NewText).

replace_complicated_words([Word/Syn|Pairs],Text,NewText) :- string_replace(Text,Word,Syn,Text1), Text\=Text1, !, replace_complicated_words([Word/Syn|Pairs],Text1,NewText).
replace_complicated_words([_|Pairs],Text,NewText) :- !, replace_complicated_words(Pairs,Text,NewText).
replace_complicated_words([],T,T).

% Some synomymns for weird legalese words SpaCy dislikes
complicatedWord(" thereof"," of it").
complicatedWord(":-",": ").
complicatedWord("\"","'"). % doublequotes seem to break parses
complicatedWord("”","'"). % ditto for smart quotes..
complicatedWord("‘’","'"). % ...and weird friends
complicatedWord("’’","'"). % ...and weird friends
complicatedWord("\t"," "). % ditto for tabs
complicatedWord("\n\n","\n"). % collapse empty lines
complicatedWord("\n","\\n"). % newlines need to be "reified" TODO: ";" would be better... at least of there's no other punctuation ending the previous line!


%%% utils
% strip leading and trailing whitespace from atom or string
strip(Atom, Stripped) :- 
	atom(Atom),
	!,
	atom_codes(Atom,Codes), strip_common(Codes,StrippedCodes), atom_codes(Stripped,StrippedCodes).
strip(String, Stripped) :- 
	string(String),
	string_codes(String,Codes), strip_common(Codes,StrippedCodes), string_codes(Stripped,StrippedCodes).

strip_common(Codes,StrippedCodes) :- strip_prefix(Codes,Codes1), strip_suffix(Codes1,StrippedCodes).
	
strip_prefix(Codes,Stripped) :- whitespace(Codes,Codes1), !, strip_prefix(Codes1,Stripped).
strip_prefix(Codes,Codes).

strip_suffix(Codes,Stripped) :- whitespace(W,[]), append(Codes1,W,Codes), !, strip_suffix(Codes1,Stripped).
strip_suffix(Codes,Codes).

whitespace([32|L],L).
whitespace([9|L],L).

string_replace(String,Pattern,Replacement,Result) :- 
	atomics_to_string(Items,Pattern,String), atomics_to_string(Items,Replacement,Result).


/** 
	count_solutions(:G,N) is det 

	count_solutions(Goal,SolutionsCount)
	Executes a goal completely, efficiently counting its solutions; better for this than using findall/3 plus length/2
*/
count_solutions(G,N) :- create_counter(C), (G, inc_counter(C), fail ; get_counter(C,N)).

create_counter(counter(0)).
get_counter(counter(N),N).
set_counter(Counter,N) :- Counter=counter(_), nb_setarg(1,Counter,N).
inc_counter(Counter,N) :- get_counter(Counter,N), NewN is N+1, nb_setarg(1,Counter,NewN).
inc_counter(Counter) :- inc_counter(Counter,_).

:- use_module(library(assoc)).
count_occurrences(L,Pairs) :- count_occurrences(L,_TopN,Pairs).

% count_occurrences(List,TopN,Pairs) Pairs is a list of ListValue-Count, ordered descendently by Count; if TopN is bound, only the TopN elements are returned 
count_occurrences(L,TopN,Pairs) :- assertion(ground(L)),
    empty_assoc(Assoc), count_occurrences_(L,Assoc,NewAssoc), assoc_to_list(NewAssoc,Pairs_),
    sort(2,@>=,Pairs_,Pairs__),
    (var(TopN) -> Pairs__=Pairs ; (length(Pairs__,N), N<TopN) -> Pairs__=Pairs; length(Pairs,TopN), append(Pairs,_,Pairs__)).

count_occurrences_([X|L],Assoc,NewAssoc) :- get_assoc(X, Assoc, Count), !, 
    NewCount is Count+1, put_assoc(X, Assoc, NewCount, Assoc2), count_occurrences_(L,Assoc2,NewAssoc).
count_occurrences_([X|L],Assoc,NewAssoc) :- !, put_assoc(X, Assoc, 1, Assoc2), !, 
    count_occurrences_(L,Assoc2,NewAssoc).
count_occurrences_([],A,A).

:- if(current_module(swish)). % On SWISH:
sandbox:safe_primitive(spacy:content(_,_)).
sandbox:safe_primitive(spacy:spaCyParseTokens(_,_,_)).
sandbox:safe_primitive(spacy:spaCyParseTokens(_,_,_,_)).
sandbox:safe_primitive(spacy:spaCyParseTokens(_,_,_,_,_,_)).
sandbox:safe_primitive(prolog_pretty_print:print_term_2(_,_)). % Somehow print_term/2 on SWISS is considered unsafe by default
sandbox:safe_primitive(spacy:depgraph(Tokens,_G)) :- is_list(Tokens).
sandbox:safe_primitive(spacy:hierplane(Tokens,_G)) :- is_list(Tokens).
sandbox:safe_primitive(spacy:sentence(_,_)).
sandbox:safe_primitive(spacy:parseAndSee(_,_,_,_,_)).
sandbox:safe_primitive(spacy:chunkVerbs(Tokens,_)) :- is_list(Tokens).
sandbox:safe_primitive(spacy:count_solutions(G,_)) :- sandbox:safe_goal(spacy:G).
sandbox:safe_primitive(spacy:count_occurrences(_,_)).
sandbox:safe_primitive(spacy:count_occurrences(_,_,_)).

% hooks for authentication
user_can_parse.
user_can_see_content.

:- else. % On command-line SWI-Prolog, no user restrictions:
user_can_parse.
user_can_see_content.
:- endif.

