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
    parse_field_key/2,
    parse_field_key/3,
    parse_table_comment/1,
    parse_select_fields/2,
    parse_fields_kvs/2,
    parse_condition/2,
    parse_like/2,
    parse_like/3,
    parse_regexp/2
]).
-include("db.hrl").

%% 转化字段名
parse_field_names(FieldIDs, TotalFields) ->
    {ok, Fields} = db_util:get_fields(FieldIDs, TotalFields),
    FieldsFormat = [io_lib:format("`~s`", [Field]) || Field <- Fields],
    {ok, string:join(FieldsFormat, ",")}.

%% 转化查询字段
parse_select_fields(FieldIDs, TotalFields) ->
    parse_select_fields_1(FieldIDs, TotalFields, [], []).

parse_select_fields_1([], _TotalFields, FieldsFormat, SelVals) ->
    {ok, string:join(FieldsFormat, ","), SelVals};
parse_select_fields_1([{K, ?DB_AS, K2} | Tail], TotalFields, OldFieldsFormat, SelVals) when is_tuple(K) -> %% 别名
    {ok, KFormat, Vals} = parse_select_fields_1([K], TotalFields, [], []),
    FieldsFormat = OldFieldsFormat ++ [io_lib:format("~ts ~s '~ts'", [KFormat, ?DB_AS, K2])],
    parse_select_fields_1(Tail, TotalFields, FieldsFormat, SelVals ++ Vals);
parse_select_fields_1([{K, ?DB_AS, K2} | Tail], TotalFields, OldFieldsFormat, SelVals) -> %% 别名
    {ok, Fields} = db_util:get_fields([K], TotalFields),
    FieldsFormat = OldFieldsFormat ++ [io_lib:format("`~s` ~s '~ts'", [Fields, ?DB_AS, K2])],
    parse_select_fields_1(Tail, TotalFields, FieldsFormat, SelVals);
parse_select_fields_1([{?DB_IF, Cond, True, False} | Tail], TotalFields, OldFieldsFormat, SelVals) -> %% 条件判断
    {ok, KFormat, Vals} = parse_if({?DB_IF, Cond, True, False}, TotalFields),
    FieldsFormat = OldFieldsFormat ++ [KFormat],
    parse_select_fields_1(Tail, TotalFields, FieldsFormat, SelVals ++ Vals);
parse_select_fields_1([{?DB_IF_NULL, Cond, Default} | Tail], TotalFields, OldFieldsFormat, SelVals) -> %% null条件判断
    {ok, KFormat, Vals} = parse_if_null({?DB_IF_NULL, Cond, Default}, TotalFields),
    FieldsFormat = OldFieldsFormat ++ [KFormat],
    parse_select_fields_1(Tail, TotalFields, FieldsFormat, SelVals ++ Vals);
parse_select_fields_1([{KFormat, K} | Tail], TotalFields, OldFieldsFormat, SelVals) when is_integer(K) ->
    {ok, Fields} = db_util:get_fields([K], TotalFields),
    FieldsFormat = OldFieldsFormat ++ [io_lib:format(KFormat, [Fields])],
    parse_select_fields_1(Tail, TotalFields, FieldsFormat, SelVals);
parse_select_fields_1([{KFormat, K} | Tail], TotalFields, OldFieldsFormat, SelVals) when is_atom(K) ->
    FieldsFormat = OldFieldsFormat ++ [io_lib:format(KFormat, [erlang:atom_to_list(K)])],
    parse_select_fields_1(Tail, TotalFields, FieldsFormat, SelVals);
parse_select_fields_1([{KFormat, K} | Tail], TotalFields, OldFieldsFormat, SelVals) when is_binary(K) ->
    FieldsFormat = OldFieldsFormat ++ [io_lib:format(KFormat, [unicode:characters_to_list(K, utf8)])],
    parse_select_fields_1(Tail, TotalFields, FieldsFormat, SelVals);
parse_select_fields_1([{KFormat, K} | Tail], TotalFields, OldFieldsFormat, SelVals) ->
    FieldsFormat = OldFieldsFormat ++ [io_lib:format(KFormat, [K])],
    parse_select_fields_1(Tail, TotalFields, FieldsFormat, SelVals);
parse_select_fields_1([FieldID | Tail], TotalFields, OldFieldsFormat, SelVals) when is_integer(FieldID) ->
    {ok, Fields} = db_util:get_fields([FieldID], TotalFields),
    FieldsFormat = OldFieldsFormat ++ [io_lib:format("`~s`", [Fields])],
    parse_select_fields_1(Tail, TotalFields, FieldsFormat, SelVals);
parse_select_fields_1([Field | Tail], TotalFields, OldFieldsFormat, SelVals) ->
    FieldsFormat = OldFieldsFormat ++ [Field],
    parse_select_fields_1(Tail, TotalFields, FieldsFormat, SelVals).

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
parse_fields(DBFields, TotalFields, PreFormat) ->
    parse_fields_2(DBFields, TotalFields, PreFormat, []).

parse_fields_2([], _TotalFields, _PreFormat, Formats) ->
    {ok, string:join(Formats, ",")};
parse_fields_2(
    [#db_field{
        field_name = FieldID,
        data_type = DataType,
        is_unsigned = IsUnsigned,
        default = Default,
        extra = Extra,
        comment = Comment,
        after_field = AfterFieldID
    } | Tail],
    TotalFields, PreFormat, Formats) ->
    {ok, [FieldName]} = db_util:get_fields([FieldID], TotalFields),
    Unsigned = ?DB_IF_(IsUnsigned, ?DB_UNSIGNED, ""),
    {ok, DefaultFormat} = parse_field_default(Default),
    {ok, CommentFormat} = parse_field_comment(Comment),
    {ok, [AfterFieldName]} = db_util:get_fields([AfterFieldID], TotalFields),
    {ok, AfterFormat} = parse_field_after(AfterFieldName),
    case Extra of
        ?DB_EXTRA_AUTO -> ExtraMsg = ?DB_EXTRA_AUTO;
        _ -> ExtraMsg = ""
    end,
    SFormat = io_lib:format(
        "~s `~s` ~s ~s ~ts ~s ~ts ~s",
        [PreFormat, FieldName, DataType, Unsigned, DefaultFormat, ExtraMsg, CommentFormat, AfterFormat]
    ),
    parse_fields_2(Tail, TotalFields, PreFormat, Formats ++ [SFormat]).

%% 转化表格描述
parse_table_comment("") ->
    {ok, "ENGINE = InnoDB CHARSET = utf8"};
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
%% null | 主键 | 唯一索引 | 普通索引
parse_field_key(DBFields, TotalFields) ->
    parse_field_key(DBFields, TotalFields, "").
parse_field_key(DBFields, TotalFields, PreFormat) ->
    parse_field_key_1(DBFields, TotalFields, PreFormat, []).

parse_field_key_1([], _TotalFields, _PreFormat, KeyFormats) ->
    {ok, string:join(KeyFormats, ",")};
parse_field_key_1([#db_field{key = null} | Tail], TotalFields, PreFormat, KeyFormats) -> %% 其他情况
    parse_field_key_1(Tail, TotalFields, PreFormat, KeyFormats);
parse_field_key_1([#db_field{field_name = FieldID, key = Key} | Tail], TotalFields, PreFormat, KeyFormats) -> %% 索引
    {ok, KFormat} = parse_field_key_2(Key, [FieldID], TotalFields, PreFormat),
    parse_field_key_1(Tail, TotalFields, PreFormat, KeyFormats ++ [KFormat]);
parse_field_key_1([{Key, FieldIDs} | Tail], TotalFields, PreFormat, KeyFormats) -> %% 索引
    {ok, KFormat} = parse_field_key_2(Key, FieldIDs, TotalFields, PreFormat),
    parse_field_key_1(Tail, TotalFields, PreFormat, KeyFormats ++ [KFormat]).

parse_field_key_2(?DB_KEY_PRI, Keys, TotalFields, PreFormat) when is_list(Keys) -> %% 主键索引
    case PreFormat of
        "drop" -> %% 删除主键索引不需要名称
            KFormat = io_lib:format("~s primary key", [PreFormat]);
        _ ->
            {ok, KeyNames} = db_util:get_fields(Keys, TotalFields),
            FieldNames = string:join([io_lib:format("`~s`", [KN]) || KN <- KeyNames], ","),
            KFormat = io_lib:format("~s primary key (~s)", [PreFormat, FieldNames])
    end,
    {ok, KFormat};
parse_field_key_2(?DB_KEY_UNI, Keys, TotalFields, PreFormat) when is_list(Keys) -> %% 唯一索引
    {ok, KeyNames} = db_util:get_fields(Keys, TotalFields),
    AliasName = string:join(KeyNames, "_"),
    case PreFormat of
        "drop" -> %% 删除唯一索引不需要唯一标识
            KFormat = io_lib:format("~s index uni_~s", [PreFormat, AliasName]);
        _ ->
            FieldNames = string:join([io_lib:format("`~s`", [KN]) || KN <- KeyNames], ","),
            KFormat = io_lib:format("~s unique index uni_~s (~s)", [PreFormat, AliasName, FieldNames])
    end,
    {ok, KFormat};
parse_field_key_2(?DB_KEY_IDX, Keys, TotalFields, PreFormat) when is_list(Keys) -> %% 普通索引
    {ok, KeyNames} = db_util:get_fields(Keys, TotalFields),
    AliasName = string:join(KeyNames, "_"),
    case PreFormat of
        "drop" -> %% 删除索引只需要索引名
            KFormat = io_lib:format("~s index idx_~s", [PreFormat, AliasName]);
        _ ->
            FieldNames = string:join([io_lib:format("`~s`", [KN]) || KN <- KeyNames], ","),
            KFormat = io_lib:format("~s index idx_~s (~s)", [PreFormat, AliasName, FieldNames])
    end,
    {ok, KFormat}.

%% 转化字段默认值
parse_field_default(null) ->
    {ok, ""};
parse_field_default("") ->
    {ok, ""};
parse_field_default(Default) when is_integer(Default) ->
    {ok, io_lib:format("not null default '~w'", [Default])};
parse_field_default(Default) when is_binary(Default) ->
    {ok, io_lib:format("not null default '~ts'", [erlang:binary_to_list(Default)])};
parse_field_default(Default) ->
    {ok, io_lib:format("not null default '~ts'", [Default])}.

%% 转化字段名 - where
parse_condition(WhereKVs, TotalFields) ->
    parse_condition_1(WhereKVs, TotalFields, [], []).

parse_condition_1([], _TotalFields, WhereFormats, WhereVals) ->
    {ok, string:join(WhereFormats, " "), WhereVals};

parse_condition_1([{K, WType, K2} | Tail], TotalFields, WhereFormats, WhereVals) when WType == ?DB_AS andalso is_tuple(K) -> %% 别名
    {ok, KFormat, Vals} = parse_select_fields_1([K], TotalFields, [], []),
    WFormat = io_lib:format("~ts ~s '~ts'", [KFormat, ?DB_AS, K2]),
    parse_condition_1(Tail, TotalFields, WhereFormats ++ [WFormat], WhereVals ++ Vals);
parse_condition_1([{K, WType, K2} | Tail], TotalFields, WhereFormats, WhereVals) when WType == ?DB_AS -> %% 别名
    {ok, Fields} = db_util:get_fields([K], TotalFields),
    WFormat = io_lib:format("`~s` ~s '~ts'", [Fields, ?DB_AS, K2]),
    parse_condition_1(Tail, TotalFields, WhereFormats ++ [WFormat], WhereVals);

parse_condition_1([WType | Tail], TotalFields, WhereFormats, WhereVals) when WType == ?DB_AND orelse WType == ?DB_OR -> %% 与 | 或
    parse_condition_1(Tail, TotalFields, WhereFormats ++ [WType], WhereVals);

parse_condition_1([{KV1, WType, KV2} | Tail], TotalFields, WhereFormats, WhereVals) when WType == ?DB_AND orelse WType == ?DB_OR -> %% 与 | 或
    {ok, KF1, V1} = parse_condition([KV1], TotalFields),
    {ok, KF2, V2} = parse_condition([KV2], TotalFields),
    WFormat = io_lib:format("(~s ~s ~s)", [KF1, WType, KF2]),
    parse_condition_1(Tail, TotalFields, WhereFormats ++ [WFormat], WhereVals ++ V1 ++ V2);

parse_condition_1([{WKey, WType, WVal} | Tail], TotalFields, WhereFormats, WhereVals) when is_map(WVal) -> %% 子查询
    {ok, WFormat, WVal2} = parse_sub_query({WKey, WType, WVal}, TotalFields),
    parse_condition_1(Tail, TotalFields, WhereFormats ++ [WFormat], WhereVals ++ WVal2);

parse_condition_1([{WKey, WType, WVal} | Tail], TotalFields, WhereFormats, WhereVals) when WType == ?DB_IN andalso is_list(WVal) -> %% 范围匹配
    {ok, WFormat} = parse_in({WKey, WType, WVal}, TotalFields),
    parse_condition_1(Tail, TotalFields, WhereFormats ++ [WFormat], WhereVals ++ WVal);

parse_condition_1([{WType, WTFormat, WVal} | Tail], TotalFields, WhereFormats, WhereVals) when WType == ?DB_LIKE -> %% 模糊匹配
    {ok, WFormat} = parse_like({WType, WTFormat, WVal}, TotalFields),
    parse_condition_1(Tail, TotalFields, WhereFormats ++ [WFormat], WhereVals);
parse_condition_1([{WKey, WType, WTFormat, WVal} | Tail], TotalFields, WhereFormats, WhereVals) when WType == ?DB_LIKE -> %% 模糊匹配
    {ok, WFormat} = parse_like({WKey, WType, WTFormat, WVal}, TotalFields),
    parse_condition_1(Tail, TotalFields, WhereFormats ++ [WFormat], WhereVals);

parse_condition_1([{WKey, WType, WTFormat, WVal} | Tail], TotalFields, WhereFormats, WhereVals) when WType == ?DB_REGEXP -> %% 正则表达式
    {ok, WFormat} = parse_regexp({WKey, WType, WTFormat, WVal}, TotalFields),
    parse_condition_1(Tail, TotalFields, WhereFormats ++ [WFormat], WhereVals);

parse_condition_1([{WKey, WType, WVal} | Tail], TotalFields, WhereFormats, WhereVals) -> %% 其他情况
    {ok, [Field]} = db_util:get_fields([WKey], TotalFields),
    WFormat = io_lib:format("`~s` ~s ?", [Field, WType]),
    parse_condition_1(Tail, TotalFields, WhereFormats ++ [WFormat], WhereVals ++ [WVal]).

%% 范围匹配
parse_in({WKey, ?DB_IN, WVal}, TotalFields) -> %% 范围匹配
    {ok, [Field]} = db_util:get_fields([WKey], TotalFields),
    RFormat = io_lib:format("`~s` ~s (~s)", [Field, ?DB_IN, string:join(lists:duplicate(length(WVal), "?"), ",")]),
    {ok, RFormat}.

%% 模糊匹配
parse_like([], _TotalFields, Format) ->
    {ok, Format};
parse_like([{?DB_LIKE, VFormat, Val} | Tail], TotalFields, Format) ->
    {ok, RFormat} = parse_like({?DB_LIKE, VFormat, Val}, TotalFields),
    parse_like(Tail, TotalFields, Format ++ [RFormat]);
parse_like([{Key, ?DB_LIKE, VFormat, Val} | Tail], TotalFields, Format) ->
    {ok, RFormat} = parse_like({Key, ?DB_LIKE, VFormat, Val}, TotalFields),
    parse_like(Tail, TotalFields, Format ++ [RFormat]).

parse_like({?DB_LIKE, VFormat, Val}, _TotalFields) ->
    RFormat = io_lib:format("~s ~s", [?DB_LIKE, io_lib:format(VFormat, [Val])]),
    {ok, RFormat};
parse_like({Key, ?DB_LIKE, VFormat, Val}, TotalFields) ->
    {ok, [Field]} = db_util:get_fields([Key], TotalFields),
    RFormat = io_lib:format("`~s` ~s ~s", [Field, ?DB_LIKE, io_lib:format(VFormat, [Val])]),
    {ok, RFormat}.

%% 条件判断
parse_if({?DB_IF, Cond, True, False}, TotalFields) ->
    case is_tuple(Cond) of
        true ->
            {ok, CondFormat, CondVals} = parse_condition([Cond], TotalFields);
        _ ->
            CondFormat = "?", CondVals = [Cond]
    end,
    case is_tuple(True) of
        true ->
            {ok, TrueFormat, TrueVals} = parse_condition([True], TotalFields);
        _ ->
            TrueFormat = "?", TrueVals = [True]
    end,
    case is_tuple(False) of
        true ->
            {ok, FalseFormat, FalseVals} = parse_condition([False], TotalFields);
        _ ->
            FalseFormat = "?", FalseVals = [False]
    end,
    RFormat = io_lib:format("~s(~ts)", [?DB_IF, string:join([CondFormat, TrueFormat, FalseFormat], ",")]),
    {ok, RFormat, CondVals ++ TrueVals ++ FalseVals}.

%% null条件判断
parse_if_null({?DB_IF_NULL, Cond, Default}, TotalFields) ->
    case is_tuple(Cond) of
        true ->
            {ok, CondFormat, CondVals} = parse_condition([Cond], TotalFields);
        _ ->
            {ok, CondFormat, CondVals} = parse_select_fields([Cond], TotalFields)
    end,
    case is_tuple(Default) of
        true ->
            {ok, DefaultFormat, DefaultVals} = parse_condition([Default], TotalFields);
        _ ->
            DefaultFormat = "?", DefaultVals = [Default]
    end,
    io:format("Cond:~p~n", [Cond]),
    io:format("CondFormat:~p~n", [CondFormat]),
    io:format("CondVals:~p~n", [CondVals]),
    RFormat = io_lib:format("~s(~ts)", [?DB_IF_NULL, string:join([CondFormat, DefaultFormat], ",")]),
    {ok, RFormat, CondVals ++ DefaultVals}.

%% 正则表达式
parse_regexp({Key, ?DB_REGEXP, Format, Val}, TotalFields) ->
    {ok, [Field]} = db_util:get_fields([Key], TotalFields),
    RFormat = io_lib:format("`~s` ~s ~s", [Field, ?DB_REGEXP, io_lib:format(Format, [Val])]),
    {ok, RFormat}.

%% 子查询
parse_sub_query({WKey, WType, #{} = WVal}, TotalFields) -> %% 子查询
    {ok, [Field]} = db_util:get_fields([WKey], TotalFields),
    {ok, WVFormat, WVal2} = db_util:select_string(WVal),
    RFormat = io_lib:format("`~s` ~s (~ts)", [Field, WType, WVFormat]),
    {ok, RFormat, WVal2}.