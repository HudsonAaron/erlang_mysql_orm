%%%-------------------------------------------------------------------
%%% @author zhuhaolin
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. 5月 2023 19:48
%%%-------------------------------------------------------------------
-module(db_filter).
-author("zhuhaolin").
%% API
-export([
    filter_show_table_format/1,                        %% 检测语句参数格式 - 获取表名
    filter_create_keys_format/1,                       %% 检测语句参数格式 - 建表语句
    filter_drop_table_format/1,                        %% 检测语句参数格式 - 删表语句
    filter_rename_table_format/1,                      %% 检测语句参数格式 - 改名语句
    filter_drop_column_format/1,                       %% 检测语句参数格式 - 删除表格字段
    filter_add_column_format/1,                        %% 检测语句参数格式 - 添加表格字段
    filter_modify_column_format/1,                     %% 检测语句参数格式 - 修改表格字段
    filter_add_modify_column_format/1,                 %% 检测语句参数格式 - 不存在就添加字段，存在则修改表格字段
    filter_add_index_format/1,                         %% 检测语句参数格式 - 添加索引
    filter_drop_index_format/1,                        %% 检测语句参数格式 - 删除索引
    filter_comment_format/1,                           %% 检测语句参数格式 - 创建表格描述
    filter_charset_collate_format/1,                   %% 检测语句参数格式 - 创建表格编码格式、校准
    filter_select_keys_format/1,                       %% 检测语句参数格式 - 查询字段名
    filter_insert_keys_format/1,                       %% 检测语句参数格式 - 插入字段名
    filter_insert_vals_format/1,                       %% 检测语句参数格式 - 插入字段数据
    filter_replace_keys_format/1,                      %% 检测语句参数格式 - 替换字段名
    filter_replace_vals_format/1,                      %% 检测语句参数格式 - 替换字段数据
    filter_update_sets_format/1,                       %% 检测语句参数格式 - 更新字段数据
    filter_delete_data_format/1,                       %% 检测语句参数格式 - 删除字段数据
    filter_truncate_table_format/1,                    %% 检测语句参数格式 - 清空表数据
    filter_where_format/1,                             %% 检测语句参数格式 - 条件匹配
    filter_group_by_format/1,                          %% 检测语句参数格式 - 查询分组
    filter_order_by_format/1,                          %% 检测语句参数格式 - 查询排序
    filter_limit_format/1                              %% 检测语句参数格式 - 查询限制数量
]).

-include("db.hrl").

%% 检测语句参数格式 - 获取表名
filter_show_table_format(QueryMap) ->
    case maps:get(?DB_SHOW_TABLES, QueryMap, []) of
        [] -> Format = "";
        Like ->
            {ok, Format} = db_parse:parse_like(Like, [], [])
    end,
    ShowFormat = io_lib:format("show tables ~ts", [Format]),
    {ok, ShowFormat}.

%% 检测语句参数格式 - 建表语句
filter_create_keys_format(QueryMap) -> %% 创建语句
    {ok, TableName} = get_table_name(QueryMap),
    %% ?DB_CREATE_KEYS := [#db_field{}]
    case maps:get(?DB_CREATE_KEYS_NOT_EXISTS, QueryMap, []) of
        [] ->
            CreateFields = maps:get(?DB_CREATE_KEYS, QueryMap, []),
            TableExtra = "";
        CreateFields ->
            TableExtra = "if not exists"
    end,
    TotalFields = maps:get(?DB_TABLE_FIELDS, QueryMap, []),
    {ok, CFieldFormat} = db_parse:parse_fields(CreateFields, TotalFields),
    %% 拼接创建表格索引
    Fields = maps:get(?DB_ADD_INDEX, QueryMap, []),
    {ok, CKeyFormat} = db_parse:parse_field_key(CreateFields ++ Fields, TotalFields),
    CreateFormat = io_lib:format(
        "create table ~s `~s` (~ts, ~ts)",
        [TableExtra, TableName, CFieldFormat, CKeyFormat]),
    {ok, CreateFormat}.

%% 检测语句参数格式 - 删表语句
filter_drop_table_format(QueryMap) -> %% 删除表格
    {ok, TableName} = get_table_name(QueryMap),
    DropTbFormat = io_lib:format("drop table ~s", [TableName]),
    {ok, DropTbFormat}.

%% 检测语句参数格式 - 改名语句
filter_rename_table_format(QueryMap) -> %% 重命名表格
    {ok, TableName} = get_table_name(QueryMap),
    {ok, NewTableName} = get_table_name(maps:get(?DB_RENAME_TABLE, QueryMap, error)),
    RenameTbFormat = io_lib:format("alter table ~s rename to ~s", [TableName, NewTableName]),
    {ok, RenameTbFormat}.

%% 检测语句参数格式 - 删除表格字段
filter_drop_column_format(QueryMap) -> %% 删除表格字段
    {ok, TableName} = get_table_name(QueryMap),
    case maps:get(?DB_DROP_COLUMN, QueryMap, []) of
        [] ->
            DropColFormat = "";
        DropColKeyIDs ->
            TotalFields = maps:get(?DB_TABLE_FIELDS, QueryMap, []),
            Fun =
                fun(FieldID) ->
                    {ok, FieldName} = db_parse:parse_field_names([FieldID], TotalFields),
                    io_lib:format("drop column ~s", [FieldName])
                end,
            DropCols = lists:map(Fun, DropColKeyIDs),
            DropColKeyFormat = string:join(DropCols, ","),
            DropColFormat = io_lib:format("alter table ~s ~s ", [TableName, DropColKeyFormat])
    end,
    {ok, DropColFormat}.

%% 检测语句参数格式 - 添加表格字段
filter_add_column_format(QueryMap) -> %% 添加表格字段
    {ok, TableName} = get_table_name(QueryMap),
    case maps:get(?DB_ADD_COLUMN, QueryMap, []) of
        [] ->
            ColFormat = "";
        Fields ->
            TotalFields = maps:get(?DB_TABLE_FIELDS, QueryMap, []),
            {ok, FieldFormat} = db_parse:parse_fields(Fields, TotalFields, "add column"),
            ColFormat = io_lib:format("alter table ~s ~ts", [TableName, FieldFormat])
    end,
    {ok, ColFormat}.

%% 检测语句参数格式 - 修改表格字段
filter_modify_column_format(QueryMap) -> %% 修改表格字段
    {ok, TableName} = get_table_name(QueryMap),
    case maps:get(?DB_MODIFY_COLUMN, QueryMap, []) of
        [] ->
            ColFormat = "";
        Fields ->
            TotalFields = maps:get(?DB_TABLE_FIELDS, QueryMap, []),
            {ok, FieldFormat} = db_parse:parse_fields(Fields, TotalFields, "modify column"),
            ColFormat = io_lib:format("alter table ~s ~ts", [TableName, FieldFormat])
    end,
    {ok, ColFormat}.

%% 检测语句参数格式 - 不存在就添加字段，存在则修改表格字段
filter_add_modify_column_format(QueryMap) -> %% 不存在就添加字段，存在则修改表格字段
    {ok, TableName} = get_table_name(QueryMap),
    case maps:get(?DB_ADD_MODIFY_COLUMN, QueryMap, []) of
        [] ->
            Fun = "";
        Fields ->
            TotalFields = maps:get(?DB_TABLE_FIELDS, QueryMap, []),
            {ok, Fun} = db_fun:fun_add_modify_column(TableName, Fields, TotalFields)
    end,
    {ok, Fun}.

%% 检测语句参数格式 - 添加索引
filter_add_index_format(QueryMap) ->
    {ok, TableName} = get_table_name(QueryMap),
    case maps:get(?DB_ADD_INDEX, QueryMap, []) of
        [] ->
            IndexFormat = "";
        Fields ->
            TotalFields = maps:get(?DB_TABLE_FIELDS, QueryMap, []),
            {ok, KeyFormat} = db_parse:parse_field_key(Fields, TotalFields, "add"),
            IndexFormat = io_lib:format("alter table ~s ~ts", [TableName, KeyFormat])
    end,
    {ok, IndexFormat}.

%% 检测语句参数格式 - 删除索引
filter_drop_index_format(QueryMap) ->
    {ok, TableName} = get_table_name(QueryMap),
    case maps:get(?DB_DROP_INDEX, QueryMap, []) of
        [] ->
            IndexFormat = "";
        Fields ->
            TotalFields = maps:get(?DB_TABLE_FIELDS, QueryMap, []),
            {ok, KeyFormat} = db_parse:parse_field_key(Fields, TotalFields, "drop"),
            IndexFormat = io_lib:format("alter table ~s ~ts", [TableName, KeyFormat])
    end,
    {ok, IndexFormat}.

%% 检测语句参数格式 - 创建表格描述
filter_comment_format(QueryMap) -> %% 创建表格描述
    case maps:get(?DB_COMMENT, QueryMap, []) of
        [] ->
            CommentFormat = "";
        CreateComment ->
            {ok, CommentFormat} = db_parse:parse_table_comment(CreateComment)
    end,
    {ok, CommentFormat}.

%% 检测语句参数格式 - 创建表格编码格式、校准
filter_charset_collate_format(QueryMap) -> %% 创建表格描述
    case maps:get(?DB_CHARSET, QueryMap, []) of
        [] ->
            CharsetFormat = "";
        CreateComment ->
            {ok, CharsetFormat} = db_parse:parse_table_charset(CreateComment)
    end,
    {ok, CharsetFormat}.

%% 检测语句参数格式 - 查询字段名
filter_select_keys_format(QueryMap) -> %% 查询字段名
    {ok, TableName} = get_table_name(QueryMap),
    case maps:get(?DB_SELECT_KEYS, QueryMap, "*") of
        "*" ->
            SelectFormat = io_lib:format("select * from ~s", [TableName]),
            Vals = [];
        SelectValIDs ->
            TotalFields = maps:get(?DB_TABLE_FIELDS, QueryMap, []),
            {ok, SelectKeys, Vals} = db_parse:parse_select_fields(SelectValIDs, TotalFields),
            SelectFormat = io_lib:format("select ~ts from ~ts", [SelectKeys, TableName])
    end,
    {ok, SelectFormat, Vals}.

%% 检测语句参数格式 - 插入字段名
filter_insert_keys_format(QueryMap) -> %% 插入字段名
    {ok, TableName} = get_table_name(QueryMap),
    case maps:get(?DB_INSERT_IGNORE_KEYS, QueryMap, []) of
        [] ->
            InsertKeyIDs = maps:get(?DB_INSERT_KEYS, QueryMap, []),
            TableExtra = "";
        InsertKeyIDs ->
            TableExtra = ?DB_IGNORE
    end,
    TotalFields = maps:get(?DB_TABLE_FIELDS, QueryMap, []),
    {ok, InsertKeys} = db_parse:parse_field_names(InsertKeyIDs, TotalFields),
    InsertFormat = io_lib:format("insert ~s into ~s (~s) values", [TableExtra, TableName, InsertKeys]),
    {ok, InsertFormat}.

%% 检测语句参数格式 - 插入字段数据
filter_insert_vals_format(QueryMap) -> %% 插入字段数据
    OldInsertVals = maps:get(?DB_INSERT_VALS, QueryMap, ""),
    case maps:get(?DB_INSERT_IGNORE_KEYS, QueryMap, maps:get(?DB_INSERT_KEYS, QueryMap, [])) of
        [] ->
            ValFormat = "",
            InsertVals = "";
        _InsertKeyIDs when length(OldInsertVals) == 0 ->
            ValFormat = "",
            InsertVals = "";
        InsertKeyIDs ->
            InsertVals = OldInsertVals,
            ValsCount = erlang:length(InsertVals),
            KeysCount = erlang:length(InsertKeyIDs),
            Count = ValsCount div KeysCount,
            {ok, ValFormat} = get_values_format(Count, KeysCount)
    end,
    {ok, ValFormat, InsertVals}.

%% 检测语句参数格式 - 替换字段名
filter_replace_keys_format(QueryMap) -> %% 替换字段名
    {ok, TableName} = get_table_name(QueryMap),
    case maps:get(?DB_REPLACE_KEYS, QueryMap, []) of
        [] ->
            ReplaceFormat = io_lib:format("replace into ~s values ", [TableName]);
        ReplaceKeyIDs ->
            TotalFields = maps:get(?DB_TABLE_FIELDS, QueryMap, []),
            {ok, ReplaceKeys} = db_parse:parse_field_names(ReplaceKeyIDs, TotalFields),
            ReplaceFormat = io_lib:format("replace into ~s (~s) values ", [TableName, ReplaceKeys])
    end,
    {ok, ReplaceFormat}.

%% 检测语句参数格式 - 替换字段数据
filter_replace_vals_format(QueryMap) -> %% 替换字段数据
    OldReplaceVals = maps:get(?DB_REPLACE_VALS, QueryMap, ""),
    case maps:get(?DB_REPLACE_KEYS, QueryMap, []) of
        [] ->
            ValFormat = "",
            ReplaceVals = "";
        _ReplaceKeyIDs when length(OldReplaceVals) == 0 ->
            ValFormat = "",
            ReplaceVals = "";
        ReplaceKeyIDs ->
            ReplaceVals = OldReplaceVals,
            ValsCount = erlang:length(ReplaceVals),
            KeysCount = erlang:length(ReplaceKeyIDs),
            Count = ValsCount div KeysCount,
            {ok, ValFormat} = get_values_format(Count, KeysCount)
    end,
    {ok, ValFormat, ReplaceVals}.

%% 检测语句参数格式 - 更新字段数据
filter_update_sets_format(QueryMap) -> %% 更新字段数据
    case maps:get(?DB_UPDATE_SETS, QueryMap, []) of
        [] ->
            SetFormat = "",
            SetVals = "";
        SetKVs ->
            {ok, TableName} = get_table_name(QueryMap),
            TotalFields = maps:get(?DB_TABLE_FIELDS, QueryMap, []),
            {ok, KeyFormat, SetVals} = db_parse:parse_fields_kvs(SetKVs, TotalFields),
            SetFormat = io_lib:format("update ~s set ~s ", [TableName, KeyFormat])
    end,
    {ok, SetFormat, SetVals}.

%% 检测语句参数格式 - 删除字段数据
filter_delete_data_format(QueryMap) -> %% 删除字段数据
    {ok, TableName} = get_table_name(QueryMap),
    DeleteFormat = io_lib:format("delete from ~s ", [TableName]),
    {ok, DeleteFormat}.

%% 检测语句参数格式 - 清空表数据
filter_truncate_table_format(QueryMap) -> %% 清空表数据
    {ok, TableName} = get_table_name(QueryMap),
    DeleteFormat = io_lib:format("truncate ~s ", [TableName]),
    {ok, DeleteFormat}.

%% 检测语句参数格式 - 条件匹配
filter_where_format(QueryMap) -> %% 条件匹配
    case maps:get(?DB_WHERE, QueryMap, []) of
        [] ->
            WhereFormat = "",
            WhereVals = "";
        WhereKVs ->
            TotalFields = maps:get(?DB_TABLE_FIELDS, QueryMap, []),
            {ok, KeyFormat, WhereVals} = db_parse:parse_condition(WhereKVs, TotalFields),
            WhereFormat = io_lib:format("where ~s", [KeyFormat])
    end,
    {ok, WhereFormat, WhereVals}.

%% 检测语句参数格式 - 查询分组
filter_group_by_format(QueryMap) -> %% 查询分组
    case maps:get(?DB_GROUP_BY, QueryMap, []) of
        [] ->
            GroupByFormat = "";
        GroupByIDs ->
            TotalFields = maps:get(?DB_TABLE_FIELDS, QueryMap, []),
            {ok, GroupByVals} = db_parse:parse_field_names(GroupByIDs, TotalFields),
            GroupByFormat = io_lib:format("group by ~s", [GroupByVals])
    end,
    {ok, GroupByFormat}.

%% 检测语句参数格式 - 查询排序
filter_order_by_format(QueryMap) -> %% 查询排序
    case maps:get(?DB_ORDER_BY, QueryMap, []) of
        [] ->
            OrderByFormat = "";
        OrderBys ->
            TotalFields = maps:get(?DB_TABLE_FIELDS, QueryMap, []),
            Fun =
                fun({OrderType, OIDs}) ->
                    {ok, OFormat} = db_parse:parse_field_names(OIDs, TotalFields),
                    OFormat ++ [" ", OrderType]
                end,
            OrderVals = lists:map(Fun, OrderBys),
            OrderVals1 = string:join(OrderVals, ","),
            OrderByFormat = io_lib:format("order by ~s", [OrderVals1])
    end,
    {ok, OrderByFormat}.

%% 检测语句参数格式 - 查询限制数量
filter_limit_format(QueryMap) -> %% 查询排序
    case maps:get(?DB_LIMIT, QueryMap, all) of
        all ->
            LimitFormat = "";
        LimitNum when is_integer(LimitNum), LimitNum >= 0 ->
            LimitFormat = io_lib:format("limit ~w", [LimitNum]);
        {LimitMin, LimitMax} when is_integer(LimitMin), LimitMin >= 0, is_integer(LimitMax), LimitMax >= 0 ->
            LimitFormat = io_lib:format("limit ~w, ~w", [LimitMin, LimitMax])
    end,
    {ok, LimitFormat}.

%% 获取表名 TODO 支持多个名字
get_table_name(#{} = QueryMap) ->
    get_table_name(maps:get(?DB_TABLE_NAME, QueryMap, error));
get_table_name(error) ->
    error;
get_table_name(TableName) when is_atom(TableName) ->
    {ok, erlang:atom_to_list(TableName)};
get_table_name(TableName) when is_list(TableName) ->
    {ok, TableName};
get_table_name(TableName) when is_binary(TableName) ->
    {ok, unicode:characters_to_list(TableName)};
get_table_name(TableName) when is_bitstring(TableName) ->
    {ok, TableName}.

%% 获取数据组
get_values_format(Count, SubCount) ->
    SubValFormat = "(" ++ string:join(lists:duplicate(SubCount, "?"), ",") ++ ")",
    ValFormat = string:join(lists:duplicate(Count, SubValFormat), ","),
    {ok, ValFormat}.
