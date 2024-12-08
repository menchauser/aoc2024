:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(lists)).
:- use_module(library(pio)).
:- use_module(library(strings)).

part1(Rules, Updates, Answer) :-
		valid_updates(Rules, Updates, ValidUpdates),
		maplist(middle_element, ValidUpdates, MiddleElements),
		sum_list(MiddleElements, Answer).

exec(Part, Path, Answer) :-
		phrase_from_file(input(Rules, Updates), Path),
		call(Part, Rules, Updates, Answer).

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

%% Input consists of: page_ordering, new line, pages
input(PageOrderRules, PageUpdates) -->
		page_order_rules(PageOrderRules),
		page_updates(PageUpdates).

% We do not use `sequence` here to avoid matching the empty line. We explicitly
% match it instead.
page_order_rules([]) --> "\n", !.
page_order_rules([Rule | Rules]) --> page_order(Rule), "\n", page_order_rules(Rules).
page_order([Page1, Page2])  --> integer(Page1), "|", integer(Page2).

page_updates(PageUpdates) --> sequence(page_update, "\n", PageUpdates).
page_update(Pages) --> sequence(integer, ",", Pages).


%% Part 1
% The rule: if Update include both pages, they should follow a rule.
% If only one page: then it's ok to skip

% If Page1 and Page2 are both in the Update and they are in wrong direction,
% then this Update is invalid, broken by InvalidRule.
update_fails_rule(Pages, [Page1, Page2]) :-
		nth1(Pos1, Pages, Page1),
		nth1(Pos2, Pages, Page2),
		Pos1 > Pos2.

% Now I want to want to find all rules which fail given update
invalid_update(Rules, Pages, FailedRules) :-
		include(update_fails_rule(Pages), Rules, FailedRules).

% Update fails at least one rule
invalid_update(_, []).
invalid_update(Rules, Pages) :-
		invalid_update(Rules, Pages, FailedRules),
		length(FailedRules, N),
		N > 0.

% Find all updates which are valid
valid_updates(Rules, Updates, ValidUpdates) :-
		exclude(invalid_update(Rules), Updates, ValidUpdates).

%! middle_element(?List:list, ?Elem)
middle_element(List, Elem) :-
		length(List, Len),
		MiddleIndex is Len // 2 + 1,
		nth1(MiddleIndex, List, Elem).


%%% Part 2
