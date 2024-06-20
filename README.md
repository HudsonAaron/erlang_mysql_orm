##**`mysql orm`操作库 `erlang`版本**
### 前言
这个工具，意在希望降低mysql开发难度

###（1）版本新增内容
+ `2024-06-20`
  - (a) 修改`not in`范围判断
  - (b) 新增`field`标识符，用于区分record字段序号和字段名
  - (c) 修复`ifnull`的默认值返回空字符`''`、`"\"\""`
+ `2024-06-06`
  - (a) 新增where参数 两个字段名进行判断运算
+ `2024-04-11`
  - (a) `json_length`，新增where参数判断json是否为空
+ `2024-03-13`
    - (a) `charset`，`collate`，新增编码格式和校准的参数选择
+ `2023-12-28`
    - (a) `from_unixtime`，更新更多语句
    - (b) 修复运算符计算函数错误
+ `2023-12-26`
    - (a) 修复`null`值的判断
+ `2023-12-05`
    - (a) `add`
    - (b) `minus`
    - (c) `multiply`
    - (d) `divide`
    - (e) `div`
    - (f) `mod`
    - (g) `round`
    - (h) `ceil`
    - (i) `floor`
    - (j) `truncate`
    - (k) `num` 保持数值
+ `2023-12-04`
    - (a) 修复`DB_SUM`使用#aaa.id时报错
+ `2023-11-06`
    - (a) 新增版本号。`db_util:version().`即可获取当前版本
+ `2023-10-19`
    - (a) `if`，支持条件判断
    - (b) `ifnull`，支持null条件判断，新增一个简化用法if_null_sum，用于判断sum结果是否为null
    - (c) `show tables`，支持查询表名列表
    - (d) `from_unixtime`
    - (e) `like`，支持更多的模糊查询的情况
    - (f) `count`
    - (g) `sum`
    - (h) `avg`
    - (i) `min`
    - (j) `max`
    - (k) `as`
    - (l) `in`
    - (m) `regexp`
    - (n) `ignore`
+ `2023-08-07`
    - (a) `auto_increment`，支持自增ID
+ `2023-08-02`
    - (a) `json`，支持json数据格式
    - (b) 支持tinyint、smallint、int、bigint默认宽度
+ `2023-07-31`
    - (a) `add primary key`，支持添加主键索引
    - (b) `add unique index`，支持添加唯一索引
    - (c) `add index`，支持添加普通索引
    - (d) `drop primary key`，支持删除主键索引
    - (e) `drop index`，支持删除唯一索引、普通索引
+ `2023-07-27`
    - (a) `select`，支持子查询，不支持联表查询等复合查询，需要mysql开源库的支持
    - (a) `alter`，支持自动识别字段，存在则修改字段，不存在则添加字段
+ `2023-05-26`
    - (a) `select`，目前不支持子查询，联表查询等复合查询
    - (b) `delete`
    - (c) `update`
    - (d) `truncate`
    - (e) `insert`
    - (f) `replace`
    - (g) `drop`
    - (h) `alter`，目前不支持添加字段和修改字段时修改字段键值类型
