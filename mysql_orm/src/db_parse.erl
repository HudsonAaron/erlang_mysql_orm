%%%-------------------------------------------------------------------
%%% @author zhuhaolin
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%    执行语句 子语句拼接
%%% @end
%%% Created : 18. 5月 2023 19:41
%%%-------------------------------------------------------------------
-module(db_parse).
-author("zhuhaolin").
%% API
-export([
    parse_field_names/2,
    parse_fields/2,
    parse_fields/3,
    parse_table_comment/1,
    parse_select_fields/2,
    parse_fields_kvs/2,
    parse_where/2
]).
-include("db.hrl").

%% 转化字段名
parse_field_names(FieldIDs, TotalFields) ->
    {ok, Fields} = db_util:get_fields(FieldIDs, TotalFields),
    FieldsFormat = [io_lib:format("`~s`", [Field]) || Field <- Fields],
    {ok, string:join(FieldsFormat, ",")}.

%% 转化查询字段
parse_select_fields(FieldIDs, TotalFields) ->
    parse_select_fields_1(FieldIDs, TotalFields, []).

parse_select_fields_1([], _TotalFields, FieldsFormat) ->
    {ok, string:join(FieldsFormat, ",")};
parse_select_fields_1([{K, ?DB_AS, K2} | Tail], TotalFields, OldFieldsFormat) when is_tuple(K) ->
    {ok, KFormat} = parse_select_fields_1([K], TotalFields, []),
    FieldsFormat = OldFieldsFormat ++ [io_lib:format("~ts ~s '~ts'", [KFormat, ?DB_AS, K2])],
    parse_select_fields_1(Tail, TotalFields, FieldsFormat);
parse_select_fields_1([{K, ?DB_AS, K2} | Tail], TotalFields, OldFieldsFormat) ->
    {ok, Fields} = db_util:get_fields([K], TotalFields),
    FieldsFormat = OldFieldsFormat ++ [io_lib:format("`~s` ~s '~ts'", [Fields, ?DB_AS, K2])],
    parse_select_fields_1(Tail, TotalFields, FieldsFormat);
parse_select_fields_1([{KFormat, K} | Tail], TotalFields, OldFieldsFormat) when is_integer(K) ->
    {ok, Fields} = db_util:get_fields([K], TotalFields),
    FieldsFormat = OldFieldsFormat ++ [io_lib:format(KFormat, [Fields])],
    parse_select_fields_1(Tail, TotalFields, FieldsFormat);
parse_select_fields_1([{KFormat, K} | Tail], TotalFields, OldFieldsFormat) when is_atom(K) ->
    FieldsFormat = OldFieldsFormat ++ [io_lib:format(KFormat, [erlang:atom_to_list(K)])],
    parse_select_fields_1(Tail, TotalFields, FieldsFormat);
parse_select_fields_1([{KFormat, K} | Tail], TotalFields, OldFieldsFormat) when is_binary(K) ->
    FieldsFormat = OldFieldsFormat ++ [io_lib:format(KFormat, [unicode:characters_to_list(K, utf8)])],
    parse_select_fields_1(Tail, TotalFields, FieldsFormat);
parse_select_fields_1([{KFormat, K} | Tail], TotalFields, OldFieldsFormat) ->
    FieldsFormat = OldFieldsFormat ++ [io_lib:format(KFormat, [K])],
    parse_select_fields_1(Tail, TotalFields, FieldsFormat);
parse_select_fields_1([FieldID | Tail], TotalFields, OldFieldsFormat) ->
    {ok, Fields} = db_util:get_fields([FieldID], TotalFields),
    FieldsFormat = OldFieldsFormat ++ [io_lib:format("`~s`", [Fields])],
    parse_select_fields_1(Tail, TotalFields, FieldsFormat).

%% 转化字段名 - kvs
parse_fields_kvs(SetKVs, TotalFields) ->
    parse_fields_kvs(SetKVs, TotalFields, [], []).
parse_fields_kvs([], _TotalFields, SetFormats, SetVals) ->
    {ok, string:join(SetFormats, ","), SetVals};
parse_fields_kvs([{FieldID, Val} | Tail], TotalFields, SetFormats, SetVals) ->
    {ok, [FieldName]} = db_util:get_fields([FieldID], TotalFields),
    SFormat = io_lib:format("`~s` = ?", [FieldName]),
    parse_fields_kvs(Tail, TotalFields, SetFormats ++ [SFormat], SetVals ++ [Val]).

%% 转化字段名
parse_fields(DBFields, TotalFields) ->
    parse_fields(DBFields, TotalFields, "").
parse_fields(DBFields, TotalFields, PrefixFormat) ->
    parse_fields_2(DBFields, TotalFields, PrefixFormat, [], []).

parse_fields_2([], _TotalFields, _PrefixFormat, Formats, KeyFormats) ->
    {ok, string:join(Formats, ","), string:join(KeyFormats, ",")};
parse_fields_2(
    [#db_field{
        field_name = FieldID,
        data_type = DataType,
        is_unsigned = IsUnsigned,
        default = Default,
        key = Key,
        comment = Comment,
        after_field = AfterFieldID
    } | Tail],
    TotalFields, PrefixFormat, Formats, OldKeyFormats) ->
    {ok, [FieldName]} = db_util:get_fields([FieldID], TotalFields),
    Unsigned = ?DB_IF(IsUnsigned, ?DB_UNSIGNED, ""),
    {ok, DefaultFormat} = parse_field_default(Default),
    {ok, CommentFormat} = parse_field_comment(Comment),
    {ok, [AfterFieldName]} = db_util:get_fields([AfterFieldID], TotalFields),
    {ok, AfterFormat} = parse_field_after(AfterFieldName),
    SFormat = io_lib:format(
        "~s `~s` ~s ~s ~s ~ts ~s",
        [PrefixFormat, FieldName, DataType, Unsigned, DefaultFormat, CommentFormat, AfterFormat]
    ),
    %% null | 主键 | 唯一索引 | 普通索引
    case parse_field_key(Key, FieldName) of
        {ok, ""} ->
            KeyFormats = OldKeyFormats;
        {ok, _KFormat} when length(PrefixFormat) > 0 -> %% add_col | modify_col 当前版本不支持修改索引
            KeyFormats = OldKeyFormats;
        {ok, KFormat} ->
            KeyFormats = OldKeyFormats ++ [KFormat]
    end,
    parse_fields_2(Tail, TotalFields, PrefixFormat, Formats ++ [SFormat], KeyFormats).

%% 转化表格描述
parse_table_comment(Comment) ->
    {ok, io_lib:format("ENGINE = InnoDB CHARSET = utf8 comment = '~ts'", [Comment])}.
%% 转化字段描述
parse_field_comment("") ->
    {ok, ""};
parse_field_comment(Comment) ->
    {ok, io_lib:format("comment '~ts'", [Comment])}.
%% 转化字段位置
parse_field_after("") ->
    {ok, ""};
parse_field_after(FieldName) ->
    {ok, io_lib:format("after `~s`", [FieldName])}.

%% 转化字段键值类型
parse_field_key(null, _FieldName) ->
    {ok, ""};
parse_field_key("", _FieldName) ->
    {ok, ""};
parse_field_key(?DB_KEY_PRI, FieldName) -> %% 主键
    parse_field_key_1("primary key (`~s`)", FieldName);
parse_field_key(?DB_KEY_UNI, FieldName) -> %% 唯一索引
    parse_field_key_2("unique index uni_~s (`~s`)", FieldName);
parse_field_key(?DB_KEY_IDX, FieldName) -> %% 普通索引
    parse_field_key_2("index idx_~s (`~s`)", FieldName).

parse_field_key_1(Format, FieldName) ->
    {ok, io_lib:format(Format, [FieldName])}.
parse_field_key_2(Format, FieldName) ->
    {ok, io_lib:format(Format, [FieldName, FieldName])}.

%% 转化字段默认值
parse_field_default(null) ->
    {ok, ""};
parse_field_default("") ->
    {ok, ""};
parse_field_default(Default) when is_integer(Default) ->
    {ok, io_lib:format("not null default '~w'", [Default])};
parse_field_default(Default) when is_binary(Default) ->
    {ok, io_lib:format("not null default '~s'", [erlang:binary_to_list(Default)])};
parse_field_default(Default) ->
    {ok, io_lib:format("not null default '~s'", [Default])}.

%% 转化字段名 - where
parse_where(WhereKVs, TotalFields) ->
    parse_where_1(WhereKVs, TotalFields, [], []).

parse_where_1([], _TotalFields, WhereFormats, WhereVals) ->
    {ok, string:join(WhereFormats, " "), WhereVals};
parse_where_1([WType | Tail], TotalFields, WhereFormats, WhereVals) when WType == ?DB_AND orelse WType == ?DB_OR -> %% 与 | 或
    parse_where_1(Tail, TotalFields, WhereFormats ++ [WType], WhereVals);
parse_where_1([{KV1, WType, KV2} | Tail], TotalFields, WhereFormats, WhereVals) when WType == ?DB_AND orelse WType == ?DB_OR -> %% 与 | 或
    {ok, KF1, V1} = parse_where([KV1], TotalFields),
    {ok, KF2, V2} = parse_where([KV2], TotalFields),
    WFormat = io_lib:format("(~s ~s ~s)", [KF1, WType, KF2]),
    parse_where_1(Tail, TotalFields, WhereFormats ++ [WFormat], WhereVals ++ V1 ++ V2);
parse_where_1([{WKey, WType, WVal} | Tail], TotalFields, WhereFormats, WhereVals) when WType == ?DB_IN -> %% 范围匹配
    {ok, [Field]} = db_util:get_fields([WKey], TotalFields),
    WFormat = io_lib:format("`~s` ~s (?)", [Field, WType]),
    parse_where_1(Tail, TotalFields, WhereFormats ++ [WFormat], WhereVals ++ [WVal]);
parse_where_1([{WKey, WType, WTFormat, WVal} | Tail], TotalFields, WhereFormats, WhereVals) when WType == ?DB_LIKE -> %% 模糊匹配
    {ok, [Field]} = db_util:get_fields([WKey], TotalFields),
    WFormat = io_lib:format("`~s` ~s ~s", [Field, WType, io_lib:format(WTFormat, [WVal])]),
    parse_where_1(Tail, TotalFields, WhereFormats ++ [WFormat], WhereVals);
parse_where_1([{WKey, WType, WVal} | Tail], TotalFields, WhereFormats, WhereVals) ->
    {ok, [Field]} = db_util:get_fields([WKey], TotalFields),
    WFormat = io_lib:format("`~s` ~s ?", [Field, WType]),
    parse_where_1(Tail, TotalFields, WhereFormats ++ [WFormat], WhereVals ++ [WVal]).