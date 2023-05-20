:- module(tokenize_opts,
          [process_options/4,
           preopts_data/3,
           tokenopts_data/3,
           postopts_data/3]).

:- use_module(library(record)).

/** <module> tokenize_opts

This is an internal module used for option processing. The predicates exported
are not meant for use by client code.
*/

% pre-processing options
:- record preopts(
       cased:boolean=false
   ).

% tokenization options
:- record tokenopts(
       numbers:boolean=true,
       strings:boolean=true
   ).

% post-processing options
:- record postopts(
       spaces:boolean=true,
       cntrl:boolean=true,
       punct:boolean=true,
       to:oneof([strings,atoms,chars,codes])=atoms,
       pack:boolean=false
   ).

%! process_options(+Options:list(term), -PreOpts:term, TokenOpts:term, -PostOpts:term) is semidet.

process_options(Options, PreOpts, TokenOpts, PostOpts) :-
    make_preopts(Options, PreOpts, Rest0),
    make_postopts(Rest0, PostOpts, Rest1),
    make_tokenopts(Rest1, TokenOpts, InvalidOpts),
    throw_on_invalid_options(InvalidOpts).

throw_on_invalid_options(InvalidOpts) :-
    InvalidOpts \= []
    -> throw(invalid_options_given(InvalidOpts))
    ;  true.
