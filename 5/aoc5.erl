-module(aoc5).
-export([do/1]).
%-compile(export_all).

%%% Check the pair rule [x,y] against the update [a,b,c]
%%% Return true if [x,y] appear in the same order in [a,b,c]
check_rule(PairRule, Update) ->
    [First, Second] = PairRule,
    UpdateRest = lists:dropwhile(fun (X) -> X /= First end, Update),
    lists:member(Second, UpdateRest).

%%% Check each of the pair rules for a single iteration against the update
%%% correcting where necessary.
check_rules_once([], Update) -> Update;
check_rules_once([PairRule | RestPairRules], Update) ->
    NewUpdate = case check_rule(PairRule, Update) of
        true -> Update;
        false ->
            [PairFirst, PairSecond] = PairRule,
            {Part1, Part2} = lists:splitwith(fun (X) -> X/=PairSecond end, Update),
            Part1 ++ [PairFirst] ++ (Part2 -- [PairFirst])
        end,
    check_rules_once(RestPairRules, NewUpdate).

%%% Check the rules repeatedly until there's no change, i.e. the order is correct
check_rules(PairRules, Update) ->
    %io:format("Old:~w~n", [Update]),
    NewUpdate = check_rules_once(PairRules, Update),
    %io:format("New:~w~n", [NewUpdate]),
    case NewUpdate==Update of
        true -> NewUpdate;
        false -> check_rules(PairRules, NewUpdate)
    end.

%%% Remove the rules from PairRules that don't apply to this update
strip_rules([], _Update, NeededRules) -> NeededRules;
strip_rules([[PairFirst, PairSecond] | PairRest], Update, NeededRules) ->
    strip_rules(PairRest, Update,
        case lists:member(PairFirst, Update) and lists:member(PairSecond, Update) of
            true -> [[PairFirst, PairSecond]] ++ NeededRules;
            false -> NeededRules
        end
    ).
strip_rules(PairRules, Update) -> strip_rules(PairRules, Update, []).

%%% Put the Update in the correct order according to the rules
generate_correct_order(Update, PairRules) ->
    %% Strip out the pair rules that don't apply to this update
    ValidPairRules = strip_rules(PairRules, Update),
    %% Generate the FullRule relevant to these pair rules
    FullPageList = lists:uniq(lists:flatten(ValidPairRules)),
    check_rules(ValidPairRules, FullPageList).

%%% Return true if the update complies with the rules
check_update(Update, PairRules) ->
    %% Update should match the modified rule exactly
    Update == generate_correct_order(Update, PairRules).

middle_element(List) ->
    lists:nth((length(List)+1) div 2, List).

do(File) ->
    code:add_path(".."),
    Contents = helpers:read_file_of_string(File),
    [RulesContents, UpdatesContents] = re:split(Contents, "\n\n"),
    Rules = helpers:string_content_to_int_rows(RulesContents),
    Updates = helpers:string_content_to_int_rows(UpdatesContents),
    io:format("Part 1: ~w~n", [lists:foldl(
            fun (Update, Sum) ->
                Sum + case check_update(Update, Rules) of
                    true -> middle_element(Update);
                    false -> 0
                end
            end,
            0,
            Updates
        )]),
    io:format("Part 2: ~w~n",[lists:foldl(
            fun (Update, Sum) ->
                Correct = generate_correct_order(Update, Rules),
                Sum + case Update == Correct of
                    true -> 0;
                    false -> middle_element(Correct)
                end
            end,
            0,
            Updates
        )]).