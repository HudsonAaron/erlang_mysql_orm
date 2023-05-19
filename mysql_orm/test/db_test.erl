%%%-------------------------------------------------------------------
%%% @author zhuhaolin
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. 5月 2023 19:33
%%%-------------------------------------------------------------------
-module(db_test).
-author("zhuhaolin").
%% API
-compile(export_all).
-include("db.hrl").

-record(aaa, {
    id = 0,
    name = ""
}).

a() ->
    SelectMap = #{
        ?DB_TABLE_FIELDS => [id, name, age, score],
        ?DB_TABLE_NAME => role,
        ?DB_SELECT_KEYS => [id, name],
        ?DB_WHERE => [?DB_NE(id, 111), ?DB_AND, ?DB_NE(name, "测试")],
        ?DB_ORDER_BY => [?DB_ASC([id])],
        ?DB_LIMIT => 1
    },
    {ok, A, B} = db_util:select_string(SelectMap),
    io:format("~p~n", [{A, B}]),
    R = mysql_poolboy:query(mypool, A, B),
    io:format("~p~n", [R]),
    ok.

b() ->
    SelectMap = #{
        ?DB_TABLE_FIELDS => [id, name, age, score],
        ?DB_TABLE_NAME => role,
        ?DB_SELECT_KEYS => [?DB_SUM(id), ?DB_MAX(name)],
        ?DB_LIMIT => 1
    },
    {ok, A, B} = db_util:select_string(SelectMap),
    io:format("~p~n", [{A, B}]),
    R = mysql_poolboy:query(mypool, A, B),
    io:format("~p~n", [R]),
    ok.

c() ->
    SelectMap = #{
        ?DB_TABLE_FIELDS => record_info(fields, aaa),
        ?DB_TABLE_NAME => role,
        ?DB_SELECT_KEYS => [#aaa.id, #aaa.name],
        ?DB_WHERE => [?DB_NE(#aaa.id, 1), ?DB_AND, ?DB_NE(#aaa.name, "测试")],
        ?DB_ORDER_BY => [?DB_ASC([#aaa.id])],
        %% ?DB_ORDER_BY => [?DB_ASC([role_id])],   %% 这个也可以
        ?DB_LIMIT => 1
    },
    {ok, A, B} = db_util:select_string(SelectMap),
    io:format("~p~n", [{A, B}]),
    R = mysql_poolboy:query(mypool, A, B),
    io:format("~p~n", [R]),
    ok.

d() ->
    SelectMap = #{
        ?DB_TABLE_FIELDS => record_info(fields, aaa),
        ?DB_TABLE_NAME => role,
        ?DB_SELECT_KEYS => [?DB_AS(#aaa.id, "ID"), #aaa.name],
        ?DB_WHERE => [?DB_NE(#aaa.id, 1), ?DB_AND, ?DB_NE(#aaa.name, "测试")],
        ?DB_ORDER_BY => [?DB_ASC([#aaa.id])],
        %% ?DB_ORDER_BY => [?DB_ASC([role_id])],   %% 这个也可以
        ?DB_LIMIT => 1
    },
    {ok, A, B} = db_util:select_string(SelectMap),
    io:format("~p~n", [{A, B}]),
    R = mysql_poolboy:query(mypool, A, B),
    io:format("~p~n", [R]),
    ok.

d2() ->
    SelectMap = #{
        ?DB_TABLE_FIELDS => record_info(fields, aaa),
        ?DB_TABLE_NAME => role,
        ?DB_SELECT_KEYS => [?DB_AS(?DB_FROM_UNIXTIME(#aaa.id), "ID"), #aaa.name],
        ?DB_WHERE => [?DB_NE(#aaa.id, 1), ?DB_AND, ?DB_NE(#aaa.name, "测试")],
        ?DB_ORDER_BY => [?DB_ASC([#aaa.id])],
        %% ?DB_ORDER_BY => [?DB_ASC([role_id])],   %% 这个也可以
        ?DB_LIMIT => 1
    },
    {ok, A, B} = db_util:select_string(SelectMap),
    io:format("~p~n", [{A, B}]),
    R = mysql_poolboy:query(mypool, A, B),
    io:format("~p~n", [R]),
    ok.

e() ->
    Map = #{
        ?DB_TABLE_FIELDS => record_info(fields, aaa),
        ?DB_TABLE_NAME => aaa,
        ?DB_CREATE_KEYS => [
            #db_field{
                field_name = #aaa.id,
                %% field_name = id, %% 这个也可以
                data_type = ?DB_TINYINT(1),
                key = ?DB_KEY_PRI,
                default = 0,
                comment = "测试ID"
            },
            #db_field{
                field_name = #aaa.name,
                data_type = ?DB_VARCHAR(20),
                comment = "测试名称"
            }
        ],
        ?DB_COMMENT => "测试表格"
    },
    {ok, A} = db_util:create_string(Map),
    io:format("~p~n", [A]),
    R = mysql_poolboy:query(mypool, A),
    io:format("~p~n", [R]),
    ok.

f() ->
    Map = #{
        ?DB_TABLE_FIELDS => record_info(fields, aaa),
        ?DB_TABLE_NAME => aaa,
        ?DB_REPLACE_KEYS => [#aaa.id, #aaa.name],
        ?DB_REPLACE_VALS => [4, "测试12", 5, "aaa"]
    },
    {ok, A, B} = db_util:replace_string(Map),
    io:format("~p~n", [A]),
    R = mysql_poolboy:query(mypool, A, B),
    io:format("~p~n", [R]),
    ok.

g() ->
    Map = #{
        ?DB_TABLE_FIELDS => record_info(fields, aaa),
        ?DB_TABLE_NAME => aaa,
        ?DB_UPDATE_SETS => [{#aaa.name, "aadsada"}],
        ?DB_WHERE => [?DB_OR(?DB_EQ(#aaa.id, 2), ?DB_EQ(#aaa.id, 5)), ?DB_AND, ?DB_EQ(#aaa.name, "测试1")]
    },
    {ok, A, B} = db_util:update_string(Map),
    io:format("~p~n", [A]),
    R = mysql_poolboy:query(mypool, A, B),
    io:format("~p~n", [R]),
    ok.

h() ->
    Map = #{
        ?DB_TABLE_FIELDS => record_info(fields, aaa),
        ?DB_TABLE_NAME => aaa,
        ?DB_DROP_COLUMN => [#aaa.name]
    },
    {ok, A} = db_util:drop_column_string(Map),
    io:format("~p~n", [A]),
    R = mysql_poolboy:query(mypool, A),
    io:format("~p~n", [R]),
    ok.

i() ->
    Map = #{
        ?DB_TABLE_NAME => aaa,
        ?DB_RENAME_TABLE => bbb
    },
    {ok, A} = db_util:rename_table_string(Map),
    io:format("~p~n", [A]),
    R = mysql_poolboy:query(mypool, A),
    io:format("~p~n", [R]),
    ok.

j() ->
    Map = #{
        ?DB_TABLE_FIELDS => record_info(fields, aaa),
        ?DB_TABLE_NAME => bbb,
        ?DB_ADD_COLUMN => [
            #db_field{
                field_name = #aaa.name,
                data_type = ?DB_VARCHAR(10),
                comment = "测试name",
                after_field = #aaa.id
            }
        ]
    },
    {ok, A} = db_util:add_column_string(Map),
    io:format("~p~n", [A]),
    R = mysql_poolboy:query(mypool, A),
    io:format("~p~n", [R]),
    ok.

k() ->
    Map = #{
        ?DB_TABLE_FIELDS => record_info(fields, aaa),
        ?DB_TABLE_NAME => bbb,
        ?DB_MODIFY_COLUMN => [
            #db_field{
                field_name = #aaa.name,
                data_type = ?DB_VARCHAR(20),
                comment = "test name",
                key = ?DB_KEY_UNI
            }
        ]
    },
    {ok, A} = db_util:modify_column_string(Map),
    io:format("~p~n", [A]),
    R = mysql_poolboy:query(mypool, A),
    io:format("~p~n", [R]),
    ok.