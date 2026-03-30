-module(swim_name).

-export([proc_name/2]).

-spec proc_name(atom(), atom()) -> atom().

proc_name(Name, Role) ->
    list_to_atom("swim_" ++ atom_to_list(Name) ++ "_" ++ atom_to_list(Role)).
