# Feature: Bracket Indexing + SQL Query Support

## Goal

```lox
var rs = sqlQuery("select name, age from customers");
print rs[0].name;   // bracket index + dot access
```

---

## Phase 1: Bracket Indexing (`a[i]`)

### Syntax

```lox
var a = newArray();
arrayPush(a, "hello");
print a[0];          // get
a[0] = "world";      // set
```

### Implementation

| Component | Change |
|-----------|--------|
| Scanner | No changes — `[` and `]` already tokenized |
| Parse rules | `TOKEN_LEFT_BRACKET` gets infix entry `(nil; subscript_; PREC_CALL)` |
| Compiler | New `subscript_(canAssign)` function — parse expression inside `[]`, emit `OP_INDEX_GET` or `OP_INDEX_SET` |
| Opcodes | `OP_INDEX_GET` (pops index + array, pushes value), `OP_INDEX_SET` (pops value + index + array, pushes value) |
| VM | Handle both opcodes — validate `isArray`, bounds check, get/set element |
| Chaining | `rs[0].name` works naturally — `subscript_` at PREC_CALL chains with `dot_` at PREC_CALL |

### Stack behavior

```
OP_INDEX_GET:  [..., array, index] → [..., value]
OP_INDEX_SET:  [..., array, index, value] → [..., value]
```

### Test: `samples/indexing.lox`

- Basic get/set
- Chained: `a[0][1]` (nested arrays)
- With dot: `records[0].name`
- Out of bounds → runtime error
- Non-array → runtime error
- Non-number index → runtime error

---

## Phase 2: SQL Query Native

### Dependencies

- Delphi ADO components (`ADODB`, `ADOInt` or `Data.Win.ADODB`)
- Connection string passed to a `sqlConnect()` native

### API

```lox
var conn = sqlConnect("Server=localhost;Database=mydb;Trusted_Connection=Yes;");
var rs = sqlQuery(conn, "select name, age from customers");
print numRows(rs);       // row count (reuse name or add sqlNumRows)
print rs[0].name;        // bracket index returns a record, dot gets field
print rs[1].age;
sqlClose(conn);
```

### Native Functions

| Native | Args | Returns | Purpose |
|--------|------|---------|---------|
| `sqlConnect(connStr)` | 1 string | connection object | Opens ADO connection |
| `sqlQuery(conn, sql)` | 2 (conn + string) | array of records | Executes SELECT, returns results |
| `sqlClose(conn)` | 1 conn | nil | Closes connection |

### New Object Kind

```
okSqlConnection
  TObjSqlConnection = record
    Obj: TObj;
    connection: TADOConnection;  // Delphi ADO connection
    isOpen: Boolean;
  end;
```

### `sqlQuery` internals

1. Validate: conn is `okSqlConnection`, sql is string, conn is open
2. Execute query via `TADOQuery` or `TADOConnection.Execute`
3. Read column names from `Fields[i].FieldName`
4. Create `TObjRecordType` at runtime: `newRecordType(colNames, colCount)`
5. For each row:
   - `newRecord(recordType)`
   - Populate fields: strings → `copyString`, numbers → `NUMBER_VAL`, nulls → `NIL_VAL`
6. Push all records into a `newArray`
7. Return the array

### GC Considerations

- `okSqlConnection`: `BlackenObject` — nothing to trace (ADO connection is non-GC Delphi object)
- `FreeObject` for `okSqlConnection`: close + free the ADO connection
- Records + array returned by `sqlQuery` use existing GC paths
- Temp allocations during query (column name strings, records, array) must be push/pop protected

### SQL Injection Prevention

- Read-only: only `SELECT` statements allowed
- Validate SQL string starts with `SELECT` (case-insensitive, trimmed)
- `TADOConnection` opened with `Mode := cmRead`
- No parameterized queries in v1 (no user-interpolated values in SQL from Lox)

### Test: `samples/sql_basic.lox`

- Connect, query, read fields, close
- Empty result set
- NULL handling
- Column type mapping (string, integer, float, null)
- Error: query on closed connection
- Error: non-SELECT statement rejected

---

## Phase Order

```
Phase 1 (bracket indexing)
  ├── Add OP_INDEX_GET, OP_INDEX_SET
  ├── Add subscript_ compiler function
  ├── Update parse rules for TOKEN_LEFT_BRACKET
  ├── VM handlers
  ├── Tests
  ├── Commit → feature/bracket-indexing
  └── Merge to dev/main

Phase 2 (SQL support)
  ├── Add okSqlConnection object kind
  ├── Add sqlConnect, sqlQuery, sqlClose natives
  ├── Runtime record type creation in sqlQuery
  ├── GC support for okSqlConnection
  ├── SQL injection guard (SELECT only, read-only mode)
  ├── Tests
  ├── Commit → feature/sql-query
  └── Merge to dev/main
```
