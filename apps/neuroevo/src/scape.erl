-module(scape).
-compile(export_all).

-include_lib("ne_common/src/include/common.hrl").
-include("records.hrl").

gen(ExoSelf_PId, Node) ->
    spawn(Node, ?MODULE, prep, [ExoSelf_PId]).

prep(ExoSelf_PId) ->
    receive
        {ExoSelf_PId, ScapeName} ->
            ScapeName:run(ExoSelf_PId)
    end.

