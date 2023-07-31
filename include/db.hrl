%%%-------------------------------------------------------------------
%%% @author zhuhaolin
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%    数据库头文件
%%% @end
%%% Created : 15. 5月 2023 19:21
%%%-------------------------------------------------------------------
-author("zhuhaolin").
-ifndef(DB_ORM).
-define(DB_ORM, true).

%% 标识符 - 与或非门
-define(DB_AND,                      "and").                             %% 并
-define(DB_AND(KV1, KV2),            {KV1, ?DB_AND, KV2}).               %% 并
-define(DB_OR,                       "or").                              %% 或
-define(DB_OR(KV1, KV2),             {KV1, ?DB_OR, KV2}).                %% 或
%% 标识符
-define(DB_IN,                       "in").                              %% 在~之中
-define(DB_IN(K, V),                 {K, ?DB_IN, V}).                    %% 在~之中
-define(DB_NIN,                      "not in").                          %% 不在~之中
-define(DB_NIN(K, V),                {K, ?DB_NIN, V}).                   %% 不在~之中

-define(DB_AS,                       "as").                              %% 别名
-define(DB_AS(K, V),                 {K, ?DB_AS, V}).                    %% 别名

-define(DB_AVG(K),                   {"avg(`~s`)", K}).                  %% 平均值
-define(DB_SUM(K),                   {"sum(`~s`)", K}).                  %% 统计K之和
-define(DB_COUNT_ALL,                {"count(~s)", "*"}).                %% 统计K行数量
-define(DB_COUNT(K),                 {"count(`~s`)", K}).                %% 统计K行数量
-define(DB_MAX(K),                   {"max(`~s`)", K}).                  %% 字段K的最大值
-define(DB_MIN(K),                   {"min(`~s`)", K}).                  %% 字段K的最小值
-define(DB_FROM_UNIXTIME(K),         {"from_unixtime(`~s`)", K}).        %% unixtime转date
-define(DB_LIKE,                     "like").                            %% 模糊匹配
-define(DB_LIKE_ALL(K, Match),       {K, ?DB_LIKE, "'~ts'", Match}).     %% 模糊匹配 - 全部
-define(DB_LIKE_MIDDLE(K, Match),    {K, ?DB_LIKE, "'%~ts%'", Match}).   %% 模糊匹配 - 中间
-define(DB_LIKE_PREFIX(K, Match),    {K, ?DB_LIKE, "'~ts%'", Match}).    %% 模糊匹配 - 前缀
-define(DB_LIKE_SUFFIX(K, Match),    {K, ?DB_LIKE, "'%~ts'", Match}).    %% 模糊匹配 - 后缀

%% 条件判断字符
-define(DB_EQ(K, V),          {K, "=", V}).                              %% 等于
-define(DB_NE(K, V),          {K, "!=", V}).                             %% 不等于
-define(DB_LT(K, V),          {K, "<", V}).                              %% 小于
-define(DB_LE(K, V),          {K, "<=", V}).                             %% 小于等于
-define(DB_GT(K, V),          {K, ">", V}).                              %% 大于
-define(DB_GE(K, V),          {K, ">=", V}).                             %% 大于等于

-define(DB_DESC(V),           {"desc", V}).                              %% 降序
-define(DB_ASC(V),            {"asc", V}).                               %% 升序

-define(DB_KEY_PRI,           "primary").                                %% 键值类型 - 主键索引
-define(DB_KEY_PRI(V),        {?DB_KEY_PRI, V}).                         %% 键值类型 - 主键索引
-define(DB_KEY_UNI,           "unique").                                 %% 键值类型 - 唯一索引
-define(DB_KEY_UNI(V),        {?DB_KEY_UNI, V}).                         %% 键值类型 - 唯一索引
-define(DB_KEY_IDX,           "index").                                  %% 键值类型 - 普通索引
-define(DB_KEY_IDX(V),        {?DB_KEY_IDX, V}).                         %% 键值类型 - 普通索引
-define(DB_UNSIGNED,          "unsigned").                               %% 无符号 - 非负整型
-define(DB_COMMENT,           "comment").                                %% 描述
-define(DB_AFTER,             "after").                                  %% 在~字段之后

-define(DB_TINYINT(Num),      io_lib:format("tinyint(~w)", [Num])).      %% 数据类型 - mini整型 (-128~127)
-define(DB_SMALLINT(Num),     io_lib:format("smallint(~w)", [Num])).     %% 数据类型 - 小整型 (-32768~32767)
-define(DB_MEDIUMINT(Num),    io_lib:format("mediumint(~w)", [Num])).    %% 数据类型 - 中整型 (-8388608~8388607)
-define(DB_INT(Num),          io_lib:format("int(~w)", [Num])).          %% 数据类型 - 整型 (-2147483648~2147483647)
-define(DB_BIGINT(Num),       io_lib:format("bigint(~w)", [Num])).       %% 数据类型 - 大整型 (-9223372036854775808~9223372036854775807)
-define(DB_CHAR(Num),         io_lib:format("char(~w)", [Num])).         %% 数据类型 - 定长字符串
-define(DB_VARCHAR(Num),      io_lib:format("varchar(~w)", [Num])).      %% 数据类型 - 可变长字符串
-define(DB_TINYTEXT,          "tinytext").                               %% 数据类型 - 可变长字符串 (0~255字节)
-define(DB_TEXT,              "text").                                   %% 数据类型 - 可变长字符串 (0~65535字节)
-define(DB_MEDIUMTEXT,        "mediumtext").                             %% 数据类型 - 可变长字符串 (0~16772150字节)
-define(DB_LONGTEXT,          "longtext").                               %% 数据类型 - 可变长字符串 (0~4294967295字节)

-define(DB_TABLE_NAME,               table_name).             %% 数据库表名
-define(DB_TABLE_FIELDS,             table_fields).           %% 数据库表字段名
-define(DB_CREATE_KEYS,              create_keys).            %% 创建表格字段名
-define(DB_CREATE_KEYS_NOT_EXISTS,   create_keys_ne).         %% 表格不存在时创建表格字段名
-define(DB_DROP_TABLE,               drop_table).             %% 删除表格
-define(DB_RENAME_TABLE,             rename_table).           %% 重命名表格
-define(DB_DROP_COLUMN,              drop_column).            %% 删除表格字段
-define(DB_ADD_COLUMN,               add_column).             %% 添加表格字段
-define(DB_MODIFY_COLUMN,            modify_column).          %% 修改表格字段
-define(DB_ADD_MODIFY_COLUMN,        add_modify_column).      %% 不存在就添加字段，存在则修改表格字段
-define(DB_ADD_INDEX,                add_index).              %% 添加索引
-define(DB_DROP_INDEX,               drop_index).             %% 删除索引
-define(DB_SELECT_KEYS,              select_keys).            %% 查询字段名
-define(DB_INSERT_KEYS,              insert_keys).            %% 插入字段名
-define(DB_INSERT_VALS,              insert_vals).            %% 插入字段数据
-define(DB_REPLACE_KEYS,             replace_keys).           %% 替换字段名
-define(DB_REPLACE_VALS,             replace_vals).           %% 替换字段数据
-define(DB_UPDATE_SETS,              update_sets).            %% 更新字段数据
-define(DB_DELETE,                   delete).                 %% 删除字段数据
-define(DB_TRUNCATE,                 truncate).               %% 清空表数据
-define(DB_WHERE,                    where).                  %% 匹配条件
-define(DB_GROUP_BY,                 group_by).               %% 分组
-define(DB_ORDER_BY,                 order_by).               %% 排序
-define(DB_LIMIT,                    limit).                  %% 限制数量

%% 字段
-record(db_field, {
    field_name = "",            %% 字段名
    data_type = "",             %% 数据类型
    is_unsigned = false,        %% 是否是无符号
    default = null,             %% 默认值
    key = null,                 %% null | primary | unique | index (当前版本 add_col | modify_col 不支持)
    comment = "",               %% 字段描述
    after_field = ""            %% 在~字段之后
}).

%% 条件判断
-define(DB_IF(Condition, TrueRet, FalseRet),
    begin
        case Condition of
            true -> TrueRet;
            _ -> FalseRet
        end
    end).
-endif.
-ifndef(DB_ORM_FIELDS).
-define(DB_ORM_FIELDS, true).
-define(DB_FIELDS(RecordName), record_info(fields, RecordName)).
-endif.