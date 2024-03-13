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
    show_table_string/1,         %% 获取表名列表语句 orm
    create_string/1,             %% 创建表格语句 orm
    drop_table_string/1,         %% 删除表格语句 orm
    rename_table_string/1,       %% 重命名表格语句 orm
    drop_column_string/1,        %% 删除表格字段语句 orm
    add_column_string/1,         %% 添加表格字段语句 orm
    modify_column_string/1,      %% 修改表格字段语句 orm
    add_modify_column_string/1,  %% 不存在就添加字段，存在则修改表格字段语句 orm
    add_index_string/1,          %% 添加索引语句 orm
    drop_index_string/1,         %% 删除索引语句 orm
    select_string/1,             %% 查询表数据语句 orm
    insert_string/1,             %% 插入表数据语句 orm
    replace_string/1,            %% 替换表数据语句 orm
    update_string/1,             %% 更新表数据语句 orm
    delete_string/1,             %% 删除表数据语句 orm
    truncate_string/1            %% 清空表数据语句 orm
]).
-export([
    version/0,                   %% 获取orm版本号
    get_fields/2                 %% 获取字段名
]).

-include("db.hrl").

%% 获取orm版本号
version() ->
    {ok, ?DB_VERSION}.

%% 获取表名列表语句 orm
show_table_string(QueryMap) ->
    {ok, ShowFormat} = db_filter:filter_show_table_format(QueryMap),
    {ok, ShowFormat}.

%% 创建表格语句 orm
create_string(QueryMap) ->
    {ok, CreateFormat} = db_filter:filter_create_keys_format(QueryMap),
    {ok, CreateComment} = db_filter:filter_comment_format(QueryMap),
    {ok, CharsetFormat} = db_filter:filter_charset_collate_format(QueryMap),
    SQLFormat = io_lib:format("~ts ~ts ~ts;", [CreateFormat, CreateComment, CharsetFormat]),
    {ok, unicode:characters_to_binary(SQLFormat, utf8)}.

%% 删除表格语句 orm
drop_table_string(QueryMap) ->
    {ok, DropTbFormat} = db_filter:filter_drop_table_format(QueryMap),
    {ok, DropTbFormat}.

%% 重命名表格语句 orm
rename_table_string(QueryMap) ->
    {ok, RenameTbFormat} = db_filter:filter_rename_table_format(QueryMap),
    {ok, RenameTbFormat}.

%% 删除表格字段语句 orm
drop_column_string(QueryMap) ->
    {ok, DropColFormat} = db_filter:filter_drop_column_format(QueryMap),
    {ok, DropColFormat}.

%% 添加表格字段语句 orm
add_column_string(QueryMap) ->
    {ok, AddColFormat} = db_filter:filter_add_column_format(QueryMap),
    {ok, unicode:characters_to_binary(AddColFormat, utf8)}.

%% 修改表格字段语句 orm
modify_column_string(QueryMap) ->
    {ok, ModifyColFormat} = db_filter:filter_modify_column_format(QueryMap),
    {ok, unicode:characters_to_binary(ModifyColFormat, utf8)}.

%% 不存在就添加字段，存在则修改表格字段语句 orm
add_modify_column_string(QueryMap) ->
    {ok, Fun} = db_filter:filter_add_modify_column_format(QueryMap),
    {ok, Fun}.

%% 添加索引语句 orm
add_index_string(QueryMap) ->
    {ok, AddIndexFormat} = db_filter:filter_add_index_format(QueryMap),
    {ok, AddIndexFormat}.

%% 删除索引语句 orm
drop_index_string(QueryMap) ->
    {ok, DropIndexFormat} = db_filter:filter_drop_index_format(QueryMap),
    {ok, DropIndexFormat}.

%% 查询表数据语句 orm
select_string(QueryMap) ->
    {ok, SelectFormat, Vals} = db_filter:filter_select_keys_format(QueryMap),
    {ok, WhereFormat, WhereVals} = db_filter:filter_where_format(QueryMap),
    {ok, GroupByFormat} = db_filter:filter_group_by_format(QueryMap),
    {ok, OrderByFormat} = db_filter:filter_order_by_format(QueryMap),
    {ok, LimitFormat} = db_filter:filter_limit_format(QueryMap),
    SQLFormat = io_lib:format(
        "~ts ~ts ~s ~s ~s",
        [SelectFormat, WhereFormat, GroupByFormat, OrderByFormat, LimitFormat]
    ),
    {ok, unicode:characters_to_binary(SQLFormat, utf8), Vals ++ WhereVals}.

%% 插入表数据语句 orm
insert_string(QueryMap) ->
    {ok, InsertFormat} = db_filter:filter_insert_keys_format(QueryMap),
    {ok, ValFormat, InsertVals} = db_filter:filter_insert_vals_format(QueryMap),
    SQLFormat = io_lib:format("~ts ~ts", [InsertFormat, ValFormat]),
    {ok, unicode:characters_to_binary(SQLFormat, utf8), InsertVals}.

%% 替换表数据语句 orm
replace_string(QueryMap) ->
    {ok, ReplaceFormat} = db_filter:filter_replace_keys_format(QueryMap),
    {ok, ValFormat, ReplaceVals} = db_filter:filter_replace_vals_format(QueryMap),
    SQLFormat = io_lib:format("~ts ~ts", [ReplaceFormat, ValFormat]),
    {ok, unicode:characters_to_binary(SQLFormat, utf8), ReplaceVals}.

%% 更新表数据语句 orm
update_string(QueryMap) ->
    {ok, UpdateFormat, SetVals} = db_filter:filter_update_sets_format(QueryMap),
    {ok, WhereFormat, WhereVals} = db_filter:filter_where_format(QueryMap),
    {ok, OrderByFormat} = db_filter:filter_order_by_format(QueryMap),
    {ok, LimitFormat} = db_filter:filter_limit_format(QueryMap),
    SQLFormat = io_lib:format(
        "~ts ~ts ~s ~s",
        [UpdateFormat, WhereFormat, OrderByFormat, LimitFormat]
    ),
    {ok, unicode:characters_to_binary(SQLFormat, utf8), SetVals ++ WhereVals}.

%% 删除表数据语句 orm
delete_string(QueryMap) ->
    {ok, DeleteFormat} = db_filter:filter_delete_data_format(QueryMap),
    {ok, WhereFormat, WhereVals} = db_filter:filter_where_format(QueryMap),
    {ok, OrderByFormat} = db_filter:filter_order_by_format(QueryMap),
    {ok, LimitFormat} = db_filter:filter_limit_format(QueryMap),
    SQLFormat = io_lib:format(
        "~s ~ts ~s ~s",
        [DeleteFormat, WhereFormat, OrderByFormat, LimitFormat]
    ),
    {ok, unicode:characters_to_binary(SQLFormat, utf8), WhereVals}.

%% 清空表数据语句 orm
truncate_string(QueryMap) ->
    {ok, TruncateFormat} = db_filter:filter_truncate_table_format(QueryMap),
    {ok, TruncateFormat}.

%% 过滤字段名列表
%% FieldIDs: [#role.sid...]
get_fields(FieldIDs, FieldNames) ->
    Fun =
        fun
            ({?DB_NUM, Num}) when is_float(Num) ->
                erlang:float_to_list(Num); %% 保留为数值
            ({?DB_NUM, Num}) when is_integer(Num) ->
                erlang:integer_to_list(Num); %% 保留为数值
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