% -*- Mode: Prolog -*-

:- use_module(library(pce)).
:- use_module(library(pce_util)).
:- use_module(library(pce_image)).
:- use_module(library(pce_tick_box)).
:- use_module(library(hyper)).
:- use_module(library('doc/load')).
:- use_module(library('plot/barchart')).
:- use_module(doc(html)).
:- use_module(doc(url_fetch)).
:- use_module(library(sgml)).
:- use_module(doc(browser)).
:- use_module(grail_xpce).

start :-
	create_options(@options),
	new(@d, dialog('Tickbox test')),
	get(@options, explicit_modes, Bool),
	get(@options, regin, Bool2),
	send(@d, append, new(@tick, tick_box('Abcd', Bool, message(@options, explicit_modes, @options?explicit_modes?negate)))),
	send(@tick, label_font, italic),
	send(@d, append, new(@tick2, tick_box('Defg', Bool2, message(@options, regin, @options?regin?negate)))),
	send(@tick2, label_font, italic),
	send(@d, append, new(_, button(close, message(@d, return, close)))),
 	send(@d, open),
	get(@d, confirm, _),
	send(@d, destroy).

end :-
	send(@d, destroy),
	send(@options, destroy),
	send(@tick, destory).

create_options(O) :-
	new(O, options(full,         % graph_mode
	               black,        % graph_color
		       white,        % graph_bgcolor
		       gray60,       % new_color
		       blue,         % active_color
		       white,        % active_bgcolor
		       'dot -Tps2 ', % graph_layout_command
		       false,        % constraint 
		       @off,         % explicit_modes
		       @on,          % uni_modal
		       @on,          % tentacle_labels
		       @off,         % zero_root
		       xpce,         % link_mode
		       'N',          % kbest_term
		       xpce,         % par_mode
		       @on,          % regin
		       @off,         % lex_by_type
		       @on,          % latex
		       a3paper,      % paper_size
		       @on,          % fxy
		       @on,          % collapse_lambda
		       @off,         % expl_brackets
		       @off,         % gv
		       @off,         % stats
		       @off,         % query
		       @off)).       % zero_heap
