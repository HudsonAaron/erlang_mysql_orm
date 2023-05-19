%%%-------------------------------------------------------------------
%%% @author zhuhaolin
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%    mysql orm 操作库
%%% @end
%%% Created : 16. 5月 2023 9:36
%%%-------------------------------------------------------------------
-module(db_util).
-author("zhuhaolin").
%% API
-export([
    create_string/1,         %% 创建表格语句 orm
    drop_table_string/1,     %% 删除表格语句 orm
    rename_table_string/1,   %% 重命名表格语句 orm
    drop_column_string/1,    %% 删除表格字段语句 orm
    add_column_string/1,     %% 添加表格字段语句 orm
    modify_column_string/1,  %% 修改表格字段语句 orm
    select_string/1,         %% 查询表数据语句 orm
    insert_string/1,         %% 插入表数据语句 orm
    replace_string/1,        %% 替换表数据语句 orm
    update_string/1,         %% 更新表数据语句 orm
    delete_string/1,         %% 删除表数据语句 orm
    truncate_string/1        %% 清空表数据语句 orm
]).
-export([
    get_fields/2             %% 获取字段名
]).

-include("db.hrl").

%% 创建表格语句 orm
create_string(QueryMap) ->
    {ok, CreateFormat} = db_filter:filter_string_format(?DB_CREATE_KEYS, QueryMap),
    {ok, CreateComment} = db_filter:filter_string_format(?DB_COMMENT, QueryMap),
    SQLFormat = io_lib:format("~ts ~ts;", [CreateFormat, CreateComment]),
    {ok, unicode:characters_to_binary(SQLFormat, utf8)}.

%% 删除表格语句 orm
drop_table_string(QueryMap) ->
    {ok, DropTbFormat} = db_filter:filter_string_format(?DB_DROP_TABLE, QueryMap),
    {ok, DropTbFormat}.

%% 重命名表格语句 orm
rename_table_string(QueryMap) ->
    {ok, RenameTbFormat} = db_filter:filter_string_format(?DB_RENAME_TABLE, QueryMap),
    {ok, RenameTbFormat}.

%% 删除表格字段语句 orm
drop_column_string(QueryMap) ->
    {ok, DropColFormat} = db_filter:filter_string_format(?DB_DROP_COLUMN, QueryMap),
    {ok, DropColFormat}.

%% 添加表格字段语句 orm
add_column_string(QueryMap) ->
    {ok, AddColFormat} = db_filter:filter_string_format(?DB_ADD_COLUMN, QueryMap),
    {ok, unicode:characters_to_binary(AddColFormat, utf8)}.

%% 修改表格字段语句 orm
modify_column_string(QueryMap) ->
    {ok, ModifyColFormat} = db_filter:filter_string_format(?DB_MODIFY_COLUMN, QueryMap),
    {ok, unicode:characters_to_binary(ModifyColFormat, utf8)}.

%% 查询表数据语句 orm
select_string(QueryMap) ->
    {ok, SelectFormat} = db_filter:filter_string_format(?DB_SELECT_KEYS, QueryMap),
    {ok, WhereFormat, WhereVals} = db_filter:filter_string_format(?DB_WHERE, QueryMap),
    {ok, GroupByFormat} = db_filter:filter_string_format(?DB_GROUP_BY, QueryMap),
    {ok, OrderByFormat} = db_filter:filter_string_format(?DB_ORDER_BY, QueryMap),
    {ok, LimitFormat} = db_filter:filter_string_format(?DB_LIMIT, QueryMap),
    SQLFormat = io_lib:format(
        "~ts ~ts ~s ~s ~s",
        [SelectFormat, WhereFormat, GroupByFormat, OrderByFormat, LimitFormat]
    ),
    {ok, unicode:characters_to_binary(SQLFormat, utf8), WhereVals}.

%% 插入表数据语句 orm
insert_string(QueryMap) ->
    {ok, InsertFormat} = db_filter:filter_string_format(?DB_INSERT_KEYS, QueryMap),
    {ok, ValFormat, InsertVals} = db_filter:filter_string_format(?DB_INSERT_VALS, QueryMap),
    SQLFormat = io_lib:format("~ts ~ts", [InsertFormat, ValFormat]),
    {ok, unicode:characters_to_binary(SQLFormat, utf8), InsertVals}.

%% 替换表数据语句 orm
replace_string(QueryMap) ->
    {ok, ReplaceFormat} = db_filter:filter_string_format(?DB_REPLACE_KEYS, QueryMap),
    {ok, ValFormat, ReplaceVals} = db_filter:filter_string_format(?DB_REPLACE_VALS, QueryMap),
    SQLFormat = io_lib:format("~ts ~ts", [ReplaceFormat, ValFormat]),
    {ok, unicode:characters_to_binary(SQLFormat, utf8), ReplaceVals}.

%% 更新表数据语句 orm
update_string(QueryMap) ->
    {ok, UpdateFormat, SetVals} = db_filter:filter_string_format(?DB_UPDATE_SETS, QueryMap),
    {ok, WhereFormat, WhereVals} = db_filter:filter_string_format(?DB_WHERE, QueryMap),
    {ok, OrderByFormat} = db_filter:filter_string_format(?DB_ORDER_BY, QueryMap),
    {ok, LimitFormat} = db_filter:filter_string_format(?DB_LIMIT, QueryMap),
    SQLFormat = io_lib:format(
        "~ts ~ts ~s ~s",
        [UpdateFormat, WhereFormat, OrderByFormat, LimitFormat]
    ),
    {ok, unicode:characters_to_binary(SQLFormat, utf8), SetVals ++ WhereVals}.

%% 删除表数据语句 orm
delete_string(QueryMap) ->
    {ok, DeleteFormat} = db_filter:filter_string_format(?DB_DELETE, QueryMap),
    {ok, WhereFormat, WhereVals} = db_filter:filter_string_format(?DB_WHERE, QueryMap),
    {ok, OrderByFormat} = db_filter:filter_string_format(?DB_ORDER_BY, QueryMap),
    {ok, LimitFormat} = db_filter:filter_string_format(?DB_LIMIT, QueryMap),
    SQLFormat = io_lib:format(
        "~s ~ts ~s ~s",
        [DeleteFormat, WhereFormat, OrderByFormat, LimitFormat]
    ),
    {ok, unicode:characters_to_binary(SQLFormat, utf8), WhereVals}.

%% 清空表数据语句 orm
truncate_string(QueryMap) ->
    {ok, TruncateFormat} = db_filter:filter_string_format(?DB_TRUNCATE, QueryMap),
    {ok, TruncateFormat}.

%% 过滤字段名列表
%% FieldIDs: [#role.sid...]
get_fields(FieldIDs, FieldNames) ->
    Fun =
        fun
            (FieldID) when is_integer(FieldID) ->
                erlang:atom_to_list(lists:nth(FieldID - 1, FieldNames));
            (FieldID) when is_atom(FieldID) ->
                erlang:atom_to_list(FieldID);
            (FieldID) when is_binary(FieldID) ->
                erlang:binary_to_list(FieldID);
            (FieldID) -> FieldID
        end,
    Fields = lists:map(Fun, FieldIDs),
    {ok, Fields}.