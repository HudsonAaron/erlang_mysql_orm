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
-define(DB_VERSION, "v1.5.3"). %% 版本 2024-06-20

-define(DB_NULL,                     "null").                            %% null值
%% 标识符 - 与或非门
-define(DB_AND,                      "and").                             %% 与
-define(DB_AND(KV1, KV2),            {KV1, ?DB_AND, KV2}).               %% 与
-define(DB_OR,                       "or").                              %% 或
-define(DB_OR(KV1, KV2),             {KV1, ?DB_OR, KV2}).                %% 或
-define(DB_NOT,                      "not").                             %% 非
%% 标识符
-define(DB_IN,                       "in").                              %% 在~之中
-define(DB_IN(K, V),                 {K, ?DB_IN, V}).                    %% 在~之中
-define(DB_NIN,                      "not in").                          %% 不在~之中
-define(DB_NIN(K, V),                {K, ?DB_NIN, V}).                   %% 不在~之中

-define(DB_AS,                       "as").                              %% 别名
-define(DB_AS(K, V),                 {K, ?DB_AS, V}).                    %% 别名

-define(DB_NUM,                      "num").                             %% 特殊标识 数值
-define(DB_NUM(A),                   {?DB_NUM, A}).                      %% 特殊标识 数值（加上这标识，则会保留为数值）

-define(DB_ADD,                      "+").                               %% 加法
-define(DB_ADD(A, B),                {A, ?DB_ADD, B}).                   %% 加法
-define(DB_MINUS,                    "-").                               %% 减法
-define(DB_MINUS(A, B),              {A, ?DB_MINUS, B}).                 %% 减法
-define(DB_MULTIPLY,                 "*").                               %% 乘法
-define(DB_MULTIPLY(A, B),           {A, ?DB_MULTIPLY, B}).              %% 乘法
-define(DB_DIVIDE,                   "/").                               %% 除法
-define(DB_DIVIDE(A, B),             {A, ?DB_DIVIDE, B}).                %% 除法
-define(DB_ROUND,                    "round").                           %% 四舍五入
-define(DB_ROUND(A),                 {?DB_ROUND, "(~s)", A}).            %% 四舍五入
-define(DB_ROUND(A, N),              {?DB_ROUND, "(~s, ~s)", A, N}).     %% 四舍五入，保留N位数
-define(DB_CEIL,                     "ceil").                            %% 向上取整
-define(DB_CEIL(A),                  {?DB_CEIL, "(~s)", A}).             %% 向上取整
-define(DB_FLOOR,                    "floor").                           %% 向下取整
-define(DB_FLOOR(A),                 {?DB_FLOOR, "(~s)", A}).            %% 向下取整
-define(DB_DIV,                      "div").                             %% 整除
-define(DB_DIV(A),                   {?DB_DIV, "(~s)", A}).              %% 整除
-define(DB_MOD,                      "mod").                             %% 取余
-define(DB_MOD(A),                   {?DB_MOD, "(~s)", A}).              %% 取余
-define(DB_TRUNCATE,                 "truncate").                        %% 截取
-define(DB_TRUNCATE(A, N),           {?DB_TRUNCATE, "(~s, ~s)", A, N}).  %% 截取

-define(DB_AVG(K),                   {"avg(`~s`)", K}).                  %% 平均值
-define(DB_SUM,                      "sum").                             %% 统计K之和
-define(DB_SUM(K),                   {?DB_SUM, "(~s)", K}).              %% 统计K之和
-define(DB_SUM_FIELD(K),             {?DB_SUM, "(`~s`)", K}).            %% 统计K之和 - 单字段

-define(DB_COUNT_ALL,                {"count(~s)", "*"}).                %% 统计K行数量
-define(DB_COUNT(K),                 {"count(`~s`)", K}).                %% 统计K行数量

-define(DB_IF,                       "if").                              %% 条件判断
-define(DB_IF(Cond, True, False),    {?DB_IF, Cond, True, False}).       %% 条件判断
-define(DB_IF_NULL,                  "ifnull").                          %% 如果null则返回默认值
-define(DB_IF_NULL(K, Default),      {?DB_IF_NULL, K, Default}).         %% 如果null则返回默认值
-define(DB_IF_NULL_SUM(K),           ?DB_IF_NULL(?DB_SUM(K), 0)).        %% 统计K之和 null返回0

-define(DB_MAX(K),                   {"max(`~s`)", K}).                  %% 字段K的最大值
-define(DB_MIN(K),                   {"min(`~s`)", K}).                  %% 字段K的最小值
-define(DB_FROM_UNIXTIME,            "from_unixtime").                   %% unixtime转date
-define(DB_FROM_UNIXTIME(K),         {?DB_FROM_UNIXTIME, K}).            %% unixtime转date
-define(DB_FROM_UNIXTIME(K, F),      {?DB_FROM_UNIXTIME, K, F}).         %% unixtime转date
-define(DB_LIKE,                     "like").                            %% 模糊匹配
-define(DB_LIKE_ALL(Match),          {?DB_LIKE, "'~ts'", Match}).        %% 模糊匹配 - 全部
-define(DB_LIKE_ALL(K, Match),       {K, ?DB_LIKE, "'~ts'", Match}).     %% 模糊匹配 - 全部
-define(DB_LIKE_MIDDLE(Match),       {?DB_LIKE, "'%~ts%'", Match}).      %% 模糊匹配 - 中间
-define(DB_LIKE_MIDDLE(K, Match),    {K, ?DB_LIKE, "'%~ts%'", Match}).   %% 模糊匹配 - 中间
-define(DB_LIKE_PREFIX(Match),       {?DB_LIKE, "'~ts%'", Match}).       %% 模糊匹配 - 前缀
-define(DB_LIKE_PREFIX(K, Match),    {K, ?DB_LIKE, "'~ts%'", Match}).    %% 模糊匹配 - 前缀
-define(DB_LIKE_SUFFIX(Match),       {?DB_LIKE, "'%~ts'", Match}).       %% 模糊匹配 - 后缀
-define(DB_LIKE_SUFFIX(K, Match),    {K, ?DB_LIKE, "'%~ts'", Match}).    %% 模糊匹配 - 后缀

-define(DB_JSON_LENGTH,              "json_length").
-define(DB_JSON_LENGTH(K, F),        {?DB_JSON_LENGTH, "(~s)", K}).             %% 计算json字段是否为空

-define(DB_REGEXP,                   "regexp").                          %% 正则表达式
-define(DB_REGEXP(K, Match),         {K, ?DB_REGEXP, "'~ts'", Match}).   %% 正则表达式 [0-9] [a-z] [a-Z] [0-9a-zA-Z]
%% SELECT table_name FROM information_schema.tables WHERE table_schema = ? AND table_name regexp 'card_[0-9]';

%% 条件判断字符
-define(DB_EQ(K, V),          {K, "=", V}).                              %% 等于
-define(DB_NE(K, V),          {K, "!=", V}).                             %% 不等于
-define(DB_LT(K, V),          {K, "<", V}).                              %% 小于
-define(DB_LE(K, V),          {K, "<=", V}).                             %% 小于等于
-define(DB_GT(K, V),          {K, ">", V}).                              %% 大于
-define(DB_GE(K, V),          {K, ">=", V}).                             %% 大于等于
-define(DB_IS(K, V),          {K, "is", V}).                             %% 是
-define(DB_NIS(K, V),         {K, "is" ++ " " ++ ?DB_NOT, V}).           %% 不是

-define(DB_DESC(V),           {"desc", V}).                              %% 降序
-define(DB_ASC(V),            {"asc", V}).                               %% 升序

-define(DB_KEY_PRI,           "primary").                                %% 键值类型 - 主键索引
-define(DB_KEY_PRI(V),        {?DB_KEY_PRI, V}).                         %% 键值类型 - 主键索引
-define(DB_KEY_UNI,           "unique").                                 %% 键值类型 - 唯一索引
-define(DB_KEY_UNI(V),        {?DB_KEY_UNI, V}).                         %% 键值类型 - 唯一索引
-define(DB_KEY_IDX,           "index").                                  %% 键值类型 - 普通索引
-define(DB_KEY_IDX(V),        {?DB_KEY_IDX, V}).                         %% 键值类型 - 普通索引
-define(DB_EXTRA_AUTO,        "auto_increment").                         %% 额外类型 - 自增ID
-define(DB_EXTRA_AUTO(V),     {?DB_EXTRA_AUTO, V}).                      %% 额外类型 - 自增ID

-define(DB_UNSIGNED,          "unsigned").                               %% 无符号 - 非负整型
-define(DB_COMMENT,           "comment").                                %% 描述
-define(DB_CHARSET,           "charset").                                %% 编码格式
-define(DB_AFTER,             "after").                                  %% 在~字段之后
-define(DB_IGNORE,            "ignore").                                 %% 忽略

-define(DB_CHARSET_COLLATE_UTF8MB3, {"utf8mb3", "utf8mb3_general_ci"}).  %% 编码格式，校准方式
-define(DB_CHARSET_COLLATE_UTF8MB4, {"utf8mb4", "utf8mb4_general_ci"}).  %% 编码格式，校准方式

-define(DB_TINYINT,           "tinyint").                                %% 数据类型 - mini整型 (-128~127)
-define(DB_TINYINT(Num),      io_lib:format("tinyint(~w)", [Num])).      %% 数据类型 - mini整型 (-128~127)
-define(DB_SMALLINT,          "smallint").                               %% 数据类型 - 小整型 (-32768~32767)
-define(DB_SMALLINT(Num),     io_lib:format("smallint(~w)", [Num])).     %% 数据类型 - 小整型 (-32768~32767)
-define(DB_MEDIUMINT,         "mediumint").                              %% 数据类型 - 中整型 (-8388608~8388607)
-define(DB_MEDIUMINT(Num),    io_lib:format("mediumint(~w)", [Num])).    %% 数据类型 - 中整型 (-8388608~8388607)
-define(DB_INT,               "int").                                    %% 数据类型 - 整型 (-2147483648~2147483647)
-define(DB_INT(Num),          io_lib:format("int(~w)", [Num])).          %% 数据类型 - 整型 (-2147483648~2147483647)
-define(DB_BIGINT,            "bigint").                                 %% 数据类型 - 大整型 (-9223372036854775808~9223372036854775807)
-define(DB_BIGINT(Num),       io_lib:format("bigint(~w)", [Num])).       %% 数据类型 - 大整型 (-9223372036854775808~9223372036854775807)
-define(DB_CHAR(Num),         io_lib:format("char(~w)", [Num])).         %% 数据类型 - 定长字符串
-define(DB_VARCHAR(Num),      io_lib:format("varchar(~w)", [Num])).      %% 数据类型 - 可变长字符串
-define(DB_TINYTEXT,          "tinytext").                               %% 数据类型 - 可变长字符串 (0~255字节)
-define(DB_TEXT,              "text").                                   %% 数据类型 - 可变长字符串 (0~65535字节)
-define(DB_MEDIUMTEXT,        "mediumtext").                             %% 数据类型 - 可变长字符串 (0~16772150字节)
-define(DB_LONGTEXT,          "longtext").                               %% 数据类型 - 可变长字符串 (0~4294967295字节)
-define(DB_JSON,              "json").                                   %% 数据类型 - 可变长字符串 (0~65535字节)

-define(DB_TABLE_NAME,                          table_name).             %% 数据库表名
-define(DB_TABLE_FIELDS,                        table_fields).           %% 数据库表字段名
-define(DB_SHOW_TABLES,                         show_tables).            %% 获取表名
-define(DB_CREATE_KEYS,                         create_keys).            %% 创建表格字段名
-define(DB_CREATE_KEYS_NOT_EXISTS,              create_keys_ne).         %% 表格不存在时创建表格字段名
-define(DB_DROP_TABLE,                          drop_table).             %% 删除表格
-define(DB_RENAME_TABLE,                        rename_table).           %% 重命名表格
-define(DB_DROP_COLUMN,                         drop_column).            %% 删除表格字段
-define(DB_ADD_COLUMN,                          add_column).             %% 添加表格字段
-define(DB_MODIFY_COLUMN,                       modify_column).          %% 修改表格字段
-define(DB_ADD_MODIFY_COLUMN,                   add_modify_column).      %% 不存在就添加字段，存在则修改表格字段
-define(DB_ADD_INDEX,                           add_index).              %% 添加索引
-define(DB_DROP_INDEX,                          drop_index).             %% 删除索引
-define(DB_SELECT_KEYS,                         select_keys).            %% 查询字段名
-define(DB_INSERT_KEYS,                         insert_keys).            %% 插入字段名
-define(DB_INSERT_IGNORE_KEYS,                  insert_ignore_keys).     %% 插入字段名，存在则不插入数据，反之插入数据
-define(DB_INSERT_VALS,                         insert_vals).            %% 插入字段数据
-define(DB_REPLACE_KEYS,                        replace_keys).           %% 替换字段名
-define(DB_REPLACE_VALS,                        replace_vals).           %% 替换字段数据
-define(DB_UPDATE_SETS,                         update_sets).            %% 更新字段数据
-define(DB_DELETE,                              delete).                 %% 删除字段数据
-define(DB_TRUNCATE_TABLE,                      truncate_table).         %% 清空表数据
-define(DB_WHERE,                               where).                  %% 匹配条件
-define(DB_GROUP_BY,                            group_by).               %% 分组
-define(DB_ORDER_BY,                            order_by).               %% 排序
-define(DB_LIMIT,                               limit).                  %% 限制数量

%% 字段
-record(db_field, {
    field_name = "",            %% 字段名
    data_type = "",             %% 数据类型
    is_unsigned = false,        %% 是否是无符号
    default = null,             %% 默认值
    key = null,                 %% null | primary | unique | index
    extra = null,               %% null | auto_increment
    comment = "",               %% 字段描述
    after_field = ""            %% 在~字段之后
}).

%% 条件判断
-define(DB_IF_(Condition, TrueRet, FalseRet),
    begin
        case Condition of
            true -> TrueRet;
            _ -> FalseRet
        end
    end).
-endif.

%% 字段列表
-ifndef(DB_ORM_FIELDS).
-define(DB_ORM_FIELDS, true).
-define(DB_FIELDS(RecordName), record_info(fields, RecordName)).
-endif.

%% 字段名
-ifndef(DB_ORM_FIELD).
-define(DB_ORM_FIELD, true).
-define(DB_FIELD, "field").
-define(DB_FIELD(Index), {"field", Index}).
-endif.