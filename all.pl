%%% Loader for running compiler with SWI prolog


:- include('swi.pl').
:- include('settings.pl').
:- include('lib/rdtok.pl').
:- include('lib/read.pl').
:- include('support.pl').
:- include('state.pl').
:- include('terms.pl').
:- include('index.pl').
:- include('dcg.pl').
:- include('macros.pl').
:- include('process.pl').
:- include('compile.pl').
:- include('assemble.pl').
:- include('xref.pl').
:- include('main.pl').

show_version_and_exit :- display('<none>\n'), halt.
