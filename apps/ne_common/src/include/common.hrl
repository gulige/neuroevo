%%%------------------------------------------------
%%% File    : common.hrl
%%% Description: 公共定义
%%%------------------------------------------------

% use lager log system: debug, info, notice, warning, error, critical, alert, emergency
-ifdef(debug).
    -define(DEBUG(F, A), lager:debug(F, A)).
    -define(DBG(F, A), lager:info(F, A)).
    -define(DBG(F), lager:info(F)).
-else.
    -define(DEBUG(F, A), ok).
    -define(DBG(F, A), ok).
    -define(DBG(F), ok).
-endif.
-define(INFO(F, A), lager:info(F, A)).
-define(INFO(F), lager:info(F)).
-define(ERR(F, A), lager:error(F, A)).
-define(ERR(F), lager:error(F)).

-define(STR(F, A), lists:flatten(io_lib:format(F, A))).

