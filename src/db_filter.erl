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
    filter_string_format/2
]).

-include("db.hrl").

%% 检测语句参数格式
filter_string_format(?DB_CREATE_KEYS, QueryMap) -> %% 创建语句
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
    {ok, CKeyFormat} = db_parse:parse_field_key(CreateFields, TotalFields),
    CreateFormat = io_lib:format(
        "create table ~s `~s` (~ts, ~ts)",
        [TableExtra, TableName, CFieldFormat, CKeyFormat]
    ),
    {ok, CreateFormat};
filter_string_format(?DB_DROP_TABLE, QueryMap) -> %% 删除表格
    {ok, TableName} = get_table_name(QueryMap),
    DropTbFormat = io_lib:format("drop table ~s", [TableName]),
    {ok, DropTbFormat};
filter_string_format(?DB_RENAME_TABLE, QueryMap) -> %% 重命名表格
    {ok, TableName} = get_table_name(QueryMap),
    {ok, NewTableName} = get_table_name(maps:get(?DB_RENAME_TABLE, QueryMap, error)),
    RenameTbFormat = io_lib:format("alter table ~s rename to ~s", [TableName, NewTableName]),
    {ok, RenameTbFormat};
filter_string_format(?DB_DROP_COLUMN, QueryMap) -> %% 删除表格字段
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
    {ok, DropColFormat};
filter_string_format(?DB_ADD_COLUMN, QueryMap) -> %% 添加表格字段
    {ok, TableName} = get_table_name(QueryMap),
    case maps:get(?DB_ADD_COLUMN, QueryMap, []) of
        [] ->
            ColFormat = "";
        Fields ->
            TotalFields = maps:get(?DB_TABLE_FIELDS, QueryMap, []),
            {ok, FieldFormat} = db_parse:parse_fields(Fields, TotalFields, "add column"),
            ColFormat = io_lib:format("alter table ~s ~ts", [TableName, FieldFormat])
    end,
    {ok, ColFormat};
filter_string_format(?DB_MODIFY_COLUMN, QueryMap) -> %% 添加表格字段
    {ok, TableName} = get_table_name(QueryMap),
    case maps:get(?DB_MODIFY_COLUMN, QueryMap, []) of
        [] ->
            ColFormat = "";
        Fields ->
            TotalFields = maps:get(?DB_TABLE_FIELDS, QueryMap, []),
            {ok, FieldFormat} = db_parse:parse_fields(Fields, TotalFields, "modify column"),
            ColFormat = io_lib:format("alter table ~s ~ts", [TableName, FieldFormat])
    end,
    {ok, ColFormat};
filter_string_format(?DB_ADD_MODIFY_COLUMN, QueryMap) -> %% 不存在就添加字段，存在则修改表格字段
    {ok, TableName} = get_table_name(QueryMap),
    case maps:get(?DB_ADD_MODIFY_COLUMN, QueryMap, []) of
        [] ->
            Fun = "";
        Fields ->
            TotalFields = maps:get(?DB_TABLE_FIELDS, QueryMap, []),
            {ok, Fun} = db_fun:fun_add_modify_column(TableName, Fields, TotalFields)
    end,
    {ok, Fun};
filter_string_format(?DB_ADD_INDEX, QueryMap) -> %% 不存在就添加字段，存在则修改表格字段
    {ok, TableName} = get_table_name(QueryMap),
    case maps:get(?DB_ADD_INDEX, QueryMap, []) of
        [] ->
            IndexFormat = "";
        Fields ->
            TotalFields = maps:get(?DB_TABLE_FIELDS, QueryMap, []),
            {ok, KeyFormat} = db_parse:parse_field_key(Fields, TotalFields, "add"),
            IndexFormat = io_lib:format("alter table ~s ~ts", [TableName, KeyFormat])
    end,
    {ok, IndexFormat};
filter_string_format(?DB_DROP_INDEX, QueryMap) -> %% 不存在就添加字段，存在则修改表格字段
    {ok, TableName} = get_table_name(QueryMap),
    case maps:get(?DB_DROP_INDEX, QueryMap, []) of
        [] ->
            IndexFormat = "";
        Fields ->
            TotalFields = maps:get(?DB_TABLE_FIELDS, QueryMap, []),
            {ok, KeyFormat} = db_parse:parse_field_key(Fields, TotalFields, "drop"),
            IndexFormat = io_lib:format("alter table ~s ~ts", [TableName, KeyFormat])
    end,
    {ok, IndexFormat};
filter_string_format(?DB_COMMENT, QueryMap) -> %% 创建表格描述
    case maps:get(?DB_COMMENT, QueryMap, []) of
        [] ->
            CommentFormat = "";
        CreateComment ->
            {ok, CommentFormat} = db_parse:parse_table_comment(CreateComment)
    end,
    {ok, CommentFormat};
filter_string_format(?DB_SELECT_KEYS, QueryMap) -> %% 查询字段名
    {ok, TableName} = get_table_name(QueryMap),
    case maps:get(?DB_SELECT_KEYS, QueryMap, "*") of
        "*" ->
            SelectFormat = io_lib:format("select * from ~s", [TableName]);
        SelectValIDs ->
            TotalFields = maps:get(?DB_TABLE_FIELDS, QueryMap, []),
            {ok, SelectKeys} = db_parse:parse_select_fields(SelectValIDs, TotalFields),
            SelectFormat = io_lib:format("select ~ts from ~ts", [SelectKeys, TableName])
    end,
    {ok, SelectFormat};
filter_string_format(?DB_INSERT_KEYS, QueryMap) -> %% 插入字段名
    {ok, TableName} = get_table_name(QueryMap),
    case maps:get(?DB_INSERT_KEYS, QueryMap, []) of
        [] ->
            InsertFormat = io_lib:format("insert into ~s values", [TableName]);
        InsertKeyIDs ->
            TotalFields = maps:get(?DB_TABLE_FIELDS, QueryMap, []),
            {ok, InsertKeys} = db_parse:parse_field_names(InsertKeyIDs, TotalFields),
            InsertFormat = io_lib:format("insert into ~s (~s) values", [TableName, InsertKeys])
    end,
    {ok, InsertFormat};
filter_string_format(?DB_INSERT_VALS, QueryMap) -> %% 插入字段数据
    OldInsertVals = maps:get(?DB_INSERT_VALS, QueryMap, ""),
    case maps:get(?DB_INSERT_KEYS, QueryMap, []) of
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
    {ok, ValFormat, InsertVals};
filter_string_format(?DB_REPLACE_KEYS, QueryMap) -> %% 替换字段名
    {ok, TableName} = get_table_name(QueryMap),
    case maps:get(?DB_REPLACE_KEYS, QueryMap, []) of
        [] ->
            ReplaceFormat = io_lib:format("replace into ~s values ", [TableName]);
        ReplaceKeyIDs ->
            TotalFields = maps:get(?DB_TABLE_FIELDS, QueryMap, []),
            {ok, ReplaceKeys} = db_parse:parse_field_names(ReplaceKeyIDs, TotalFields),
            ReplaceFormat = io_lib:format("replace into ~s (~s) values ", [TableName, ReplaceKeys])
    end,
    {ok, ReplaceFormat};
filter_string_format(?DB_REPLACE_VALS, QueryMap) -> %% 替换字段数据
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
    {ok, ValFormat, ReplaceVals};
filter_string_format(?DB_UPDATE_SETS, QueryMap) -> %% 更新字段数据
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
    {ok, SetFormat, SetVals};
filter_string_format(?DB_DELETE, QueryMap) -> %% 删除字段数据
    {ok, TableName} = get_table_name(QueryMap),
    DeleteFormat = io_lib:format("delete from ~s ", [TableName]),
    {ok, DeleteFormat};
filter_string_format(?DB_TRUNCATE, QueryMap) -> %% 清空表数据
    {ok, TableName} = get_table_name(QueryMap),
    DeleteFormat = io_lib:format("truncate ~s ", [TableName]),
    {ok, DeleteFormat};
filter_string_format(?DB_WHERE, QueryMap) -> %% 条件匹配
    case maps:get(?DB_WHERE, QueryMap, []) of
        [] ->
            WhereFormat = "",
            WhereVals = "";
        WhereKVs ->
            TotalFields = maps:get(?DB_TABLE_FIELDS, QueryMap, []),
            {ok, KeyFormat, WhereVals} = db_parse:parse_where(WhereKVs, TotalFields),
            WhereFormat = io_lib:format("where ~s", [KeyFormat])
    end,
    {ok, WhereFormat, WhereVals};
filter_string_format(?DB_GROUP_BY, QueryMap) -> %% 查询分组
    case maps:get(?DB_GROUP_BY, QueryMap, []) of
        [] ->
            GroupByFormat = "";
        GroupByIDs ->
            TotalFields = maps:get(?DB_TABLE_FIELDS, QueryMap, []),
            {ok, GroupByVals} = db_parse:parse_field_names(GroupByIDs, TotalFields),
            GroupByFormat = io_lib:format("group by ~s", [GroupByVals])
    end,
    {ok, GroupByFormat};
filter_string_format(?DB_ORDER_BY, QueryMap) -> %% 查询排序
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
    {ok, OrderByFormat};
filter_string_format(?DB_LIMIT, QueryMap) -> %% 查询排序
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
