%%%-------------------------------------------------------------------
%%% @author zhuhaolin
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. 7月 2023 14:28
%%%-------------------------------------------------------------------
-module(db_fun).
-author("zhuhaolin").
%% API
-export([
    fun_add_modify_column/3
]).

-include("db.hrl").

%% 获取添加修改字段函数，如果表格存在字段，则修改字段属性，如果不存在字段，则添加字段
-spec fun_add_modify_column(
    TableName :: string(),
    DBFields :: list(),
    TotalFields :: list()
) ->
    {ok, Fun :: function()}.
fun_add_modify_column(TableName, DBFields, TotalFields) ->
    SelectColumnSQL = lists:concat(["show columns from ", TableName, ";"]),
    AlterFormat = lists:concat(["alter table ", TableName]),
    Fun =
    fun(Pid) ->
        case mysql:query(Pid, SelectColumnSQL) of
            {ok, _, OldColumnList} ->
                KeyList = [Key || [Key | _] <- OldColumnList],
                {ok, FieldFormat} = filter_add_modify_list(DBFields, TotalFields, KeyList, []),
                AlterSQL = io_lib:format("~s ~ts;", [AlterFormat, FieldFormat]),
                io:format("~ts~n~n", [unicode:characters_to_binary(AlterSQL, utf8)]),
                ok = mysql:query(Pid, unicode:characters_to_binary(AlterSQL, utf8)),
                ok;
            Error -> %% 报错
                {error, Error}
        end
    end,
    {ok, Fun}.

filter_add_modify_list([], _TotalFields, _KeyList, FieldFormats) ->
    {ok, string:join(FieldFormats, ",")};
filter_add_modify_list([#db_field{field_name = FieldID} = DBField | Tail], TotalFields, KeyList, FieldFormats) ->
    {ok, [FieldName]} = db_util:get_fields([FieldID], TotalFields),
    case lists:member(erlang:list_to_binary(FieldName), KeyList) of
        true -> %% 已经存在，修改字段
            {ok, FieldFormat} = db_parse:parse_fields([DBField], TotalFields, "modify column");
        _ -> %% 字段不存在，添加
            {ok, FieldFormat} = db_parse:parse_fields([DBField], TotalFields, "add column")
    end,
    filter_add_modify_list(Tail, TotalFields, KeyList, FieldFormats ++ [FieldFormat]).