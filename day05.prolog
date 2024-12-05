:- use_module(library(strings)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(pio)).


% How to test:
%   test_input(Input), part1(Input, Answer).
test_page_update(S) :- string_chars("75,29,13", S).
test_page_updates(S) :- 
		S = {|string||
				 |42,12
				 |19,15,99|}.
test_page_orders(S) :- 
		S = {|string||
				 |42|12
				 |19|15|}.
test_input(S) :- 
		S = {|string||
				 |42|12
				 |19|15
				 |
				 |75,12,31
				 |98,12|}.

%! Input consists of: page_ordering, new line, pages
input(PageOrderRules, PageUpdates) -->
		page_order_rules(PageOrderRules),
		page_updates(PageUpdates).

% We do not use `sequence` here to avoid matching the empty line. We explicitly
% match it instead.
page_order_rules([]) --> "\n", !.
page_order_rules([Rule | Rules]) --> page_order(Rule), "\n", page_order_rules(Rules).
page_order(order(Page1, Page2))  --> integer(Page1), "|", integer(Page2).

page_updates(PageUpdates) --> sequence(page_update, "\n", PageUpdates).
page_update(Pages) --> sequence(integer, ",", Pages).


%! valid_update(?Rules:list, ?Update:list)
%  Update consists of page numbers and is valid agains Rules.
valid_update(Rules, [Page | Rest]) :- valid_page(Rules, Page, Rest).

% page is valid in the update list if Rules are considered valid
