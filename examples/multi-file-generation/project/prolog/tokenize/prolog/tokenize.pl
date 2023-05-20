:- module(tokenize,
          [ tokenize/2,
            tokenize/3,
            tokenize_file/2,
            tokenize_file/3,
            untokenize/2
          ]).

/** <module> tokenize

This module offers a simple tokenizer with flexible options.

@author Shon Feder
@license <http://unlicense.org/>

Rational:

tokenize_atom/2, in library(porter_stem), is inflexible, in that it doesn't
allow for the preservation of white space or control characters, and it only
tokenizes into a list of atoms.

The `tokenize` library is meant to be easy to use while allowing for relatively
flexible input and output. Features include

  * options for tokenization of spaces, numbers, strings, control characters and punctuation
  * options to output packed tokens
  * options to represent tokens in any of the common SWI-Prolog text formats
  * option to preserve or ignore case
  * a predicate to emit text given a list of tokens

E.g.,

==
?- tokenize('Tokenizes: words,"strings", 1234.5\n', Tokens, [cased(true), spaces(false)]),
|    untokenize(Tokens, Codes).
Tokens = [word('Tokenizes'), punct(:), word(words), punct(','), string(strings), punct(','), number(1234.5), cntrl('\n')],
Codes = "Tokenizes:words,"strings"...34.5
".
==

`tokenize` is much more limited and much less performant than a lexer generator,
but it is dead simple to use and flexible enough for many common use cases.
*/

:- use_module(library(dcg/basics), [eos//0, number//1]).
:- use_module(tokenize_opts).

% Ensure we interpret back ticks as enclosing code lists in this module.
:- set_prolog_flag(back_quotes, codes).

%! tokenize(+Text:text, -Tokens:list(term)) is semidet.
%
%   @see tokenize/3 when called with an empty list of options: thus, with defaults.

% TODO: add support for unicode

tokenize(Text, Tokens) :-
    tokenize(Text, Tokens, []).

%! tokenize(+Text:text, -Tokens:list(term), +Options:list(term)) is semidet.
%
%  True when Tokens is unified with a list of tokens representing the text from
%  Text, according to the options specified in Options.
%
%  Each token in Tokens will be one of:
%
%   * word(W)
%     Where W is comprised of contiguous alpha-numeric chars.
%   * punct(P)
%     Where char_type(P, punct).
%   * cntrl(C)
%     Where char_type(C, cntrl).
%   * space(S)
%     Where `S == ' '`.
%   * number(N)
%     Where number(N).
%   * string(S)
%     Where S was a sequence of bytes enclosed by double quotation marks.
%
%  Note that the above describes the default behavior, in which the token is
%  represented as an `atom`. This representation can be changed by using the
%  `to` option described below.
%
%  Valid Options are:
%
%   * cased(+boolean)
%     Determines whether tokens perserve cases of the source text. Defaults to `cased(false)`.
%   * spaces(+boolean)
%     Determines whether spaces are represted as tokens or discarded. Defaults to `spaces(true)`.
%   * cntrl(+boolean)
%     Determines whether control characters are represented as tokens or discarded. Defaults to `cntrl(true)`.
%   * punct(+boolean)
%     Determines whether punctuation characters are represented as tokens or discarded. Defaults to `punct(true)`.
%   * numbers(+boolean)
%     Determines whether the tokenizer represents and tags numbers. Defaults to `numbers(true)`.
%   * strings(+boolean)
%     Determines whether the tokenizer represents and tags strings. Defaults to `strings(true)`.
%   * pack(+boolean)
%     Determines whether tokens are packed or repeated. Defaults to `pack(false)`.
%   * to(+one_of([strings,atoms,chars,codes]))
%     Determines the representation format used for the tokens. Defaults to `to(atoms)`.

% TODO is it possible to achieve the proper semidet without the cut?
% Annie sez some parses are ambiguous, not even sure the cut should be
% there

tokenize(Text, ProcessedTokens, Options) :-
    must_be(nonvar, Text),
    string_codes(Text, Codes),
    process_options(Options, PreOpts, TokenOpts, PostOpts),
    preprocess(PreOpts, Codes, ProcessedCodes),
    phrase(tokens(TokenOpts, Tokens), ProcessedCodes),
    postprocess(PostOpts, Tokens, ProcessedTokens),
    !.

non_tokens([T])    --> T.
non_tokens([T|Ts]) --> T, non_tokens(Ts).

%! tokenize_file(+File:atom, -Tokens:list(term)) is semidet.
%
%   @see tokenize_file/3 when called with an empty list of options: thus, with defaults.
%

% Note: does not use phrase_from_file/3, thus not lazy or transparent
% This choice was made so that tokenize_file will work with remotely
% accessed files.
% TODO: make this configurable, so it can be used in the different modes

% TODO: add more source options

tokenize_file(File, Tokens) :-
    tokenize_file(File, Tokens, []).

%! tokenize_file(+File:atom, -Tokens:list(term), +Options:list(term)) is semidet.
%
%   True when Tokens is unified with a list of tokens represening
%   the text of File.
%
%   @see tokenize/3 which has the same available options and behavior.

tokenize_file(File, Tokens, Options) :-
    read_file_to_codes(File, Codes, [encoding(utf8)]),
    tokenize(Codes, Tokens, Options).

%! untokenize(+Tokens:list(term), -Untokens:list(codes)) is semidet.
%
%   True when Untokens is unified with a code list representation of each
%   token in Tokens.

% TODO structure(Options:[lines, brackets])
% TODO mode(generate) ; mode(parse)
% TODO add output format option
% TODO is it possible to achieve the proper semidet  without the cut?

untokenize(Tokens, Untokens) :-
    untokenize(Tokens, Untokens, []).
untokenize(Tokens, Untokens, _Options) :-
    maplist(token_to(codes), Tokens, TokenCodes),
    phrase(non_tokens(TokenCodes), Untokens),
    !.

/***********************************
*      {PRE,POST}-PROCESSING HELPERS      *
***********************************/

preprocess(PreOpts, Codes, ProcessedCodes) :-
    preopts_data(cased, PreOpts, Cased),
    DCG_Rules = (
        preprocess_case(Cased)
    ),
    phrase(process_dcg_rules(DCG_Rules, ProcessedCodes), Codes).

postprocess(PostOpts, Tokens, ProcessedTokens) :-
    postopts_data(spaces, PostOpts, Spaces),
    postopts_data(cntrl, PostOpts, Cntrl),
    postopts_data(punct, PostOpts, Punct),
    postopts_data(to, PostOpts, To),
    postopts_data(pack, PostOpts, Pack),
    DCG_Rules = (
        keep_token(space(_), Spaces),
        keep_token(cntrl(_), Cntrl),
        keep_token(punct(_), Punct),
        convert_token(To)
    ),
    phrase(process_dcg_rules(DCG_Rules, PrePackedTokens), Tokens),
    (Pack
    -> phrase(pack_tokens(ProcessedTokens), PrePackedTokens)
    ;  ProcessedTokens = PrePackedTokens
    ).


/***********************************
*      POSTPROCESSING HELPERS      *
***********************************/

% Process a stream through a pipeline of DCG rules
process_dcg_rules(_, []) --> eos, !.
process_dcg_rules(DCG_Rules, []) --> DCG_Rules, eos, !.
process_dcg_rules(DCG_Rules, [C|Cs]) -->
    DCG_Rules,
    [C],
    process_dcg_rules(DCG_Rules, Cs).

preprocess_case(true), [C] --> [C].
preprocess_case(false), [CodeOut] --> [CodeIn],
    { to_lower(CodeIn, CodeOut) }.

keep_token(_, true), [T] --> [T].
keep_token(Token, false) --> [Token].
keep_token(Token, false), [T] --> [T], {T \= Token}.

convert_token(Type), [Converted] --> [Token],
    {token_to(Type, Token, Converted)}.

% Convert tokens to alternative representations.
token_to(_, number(X), number(X)) :- !.
token_to(Type, Token, Converted) :-
    ( Type == strings -> Conversion = inverse(string_codes)
    ; Type == atoms   -> Conversion = inverse(atom_codes)
    ; Type == chars   -> Conversion = inverse(string_chars)
    ; Type == codes   -> Conversion = string_codes
    ),
    call_into_term(Conversion, Token, Converted).

% Packing repeating tokens
pack_tokens([T])    --> pack_token(T).
pack_tokens([T|Ts]) --> pack_token(T), pack_tokens(Ts).

pack_token(P) --> pack(Token, N), {Token =.. [F,T], P =.. [F,T,N]}.

pack(X, Count) --> [X], pack(X, 1, Count).

pack(_, Total, Total)      --> eos.
pack(X, Total, Total), [Y] --> [Y], { Y \= X }.
pack(X, Count, Total)      --> [X], { succ(Count, NewCount) },
                               pack(X, NewCount, Total).


/**************************
*      TOKENIZATION       *
**************************/

tokenize_text --> state(Text, Tokenized),
                  { phrase(tokens(Tokenized), Text) }.


% PARSING

tokens(Opts, [T])    --> token(Opts, T), eos, !.
tokens(Opts, [T|Ts]) --> token(Opts, T), tokens(Opts, Ts).

% NOTE for debugging
% tokens(_)   --> {length(L, 200)}, L, {format(L)}, halt, !.

token(Opts, string(S)) -->
    { tokenopts_data(strings, Opts, true) },
    string(S).

token(Opts, number(N)) -->
    { tokenopts_data(numbers, Opts, true) },
    number(N), !.

token(_Opts, word(W))     --> word(W), eos, !.
token(_Opts, word(W)),` ` --> word(W), ` `.
token(_Opts, word(W)), C  --> word(W), (punct(C) ; cntrl(C) ; nasciis(C)).

token(_Opts, space(S))   --> space(S).
token(_Opts, punct(P)) --> punct(P).
token(_Opts, cntrl(C)) --> cntrl(C).
token(_Opts, other(O)) --> nasciis(O).

space(` `) --> ` `.

sep --> ' '.
sep --> eos, !.

word(W) --> csyms(W).

% TODO Make open and close brackets configurable
string(S) --> string(`"`, `"`, S).
string(OpenBracket, CloseBracket, S) --> string_start(OpenBracket, CloseBracket, S).

% A string starts when we encounter an OpenBracket
string_start(OpenBracket, CloseBracket, Cs) -->
    OpenBracket, string_content(OpenBracket, CloseBracket, Cs).

% String content is everything up until we hit a CloseBracket
string_content(_OpenBracket, CloseBracket, []) --> CloseBracket, !.
% String content includes a bracket following an escape, but not the escape
string_content(OpenBracket, CloseBracket, [C|Cs]) -->
    escape, (CloseBracket | OpenBracket),
    {[C] = CloseBracket},
    string_content(OpenBracket, CloseBracket, Cs).
% String content includes any character that isn't a CloseBracket or an escape.
string_content(OpenBracket, CloseBracket, [C|Cs]) -->
    [C],
    {[C] \= CloseBracket},
    string_content(OpenBracket, CloseBracket, Cs).

csyms([L])    --> csym(L).
csyms([L|Ls]) --> csym(L), csyms(Ls).

csym(L)       --> [L], {code_type(L, csym)}.


% non ascii's
nasciis([C])     --> nascii(C), eos, !.
nasciis([C]),[D] --> nascii(C), [D], {D < 127}.
nasciis([C|Cs])  --> nascii(C), nasciis(Cs).

nascii(C)        --> [C], {C > 127}.

' ' --> space.
' ' --> space, ' '.

escape --> `\\`.

% Any
... --> [].
... --> [_], ... .

space --> [S], {code_type(S, white)}.

punct([P]) --> [P], {code_type(P, punct)}.
cntrl([C]) --> [C], {code_type(C, cntrl)}.

% TODO move to general module

codes_to_lower([], []).
codes_to_lower([U|Uppers], [L|Lowers]) :-
    code_type(U, to_upper(L)),
    codes_to_lower(Uppers, Lowers).

call_into_term(P, Term, Result) :-
    Term =.. [F, Arg],
    call(P, Arg, ResultArg),
    Result =.. [F, ResultArg].

inverse(P, A, B) :-
    call(P, B, A).

pad(T_Args, X, T_X_Args) :-
    T_Args   =.. [T|Args],
    T_X_Args =.. [T, X| Args].
