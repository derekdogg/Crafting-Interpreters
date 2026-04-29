# Scanner (Lexer)

The scanner converts raw source text into a stream of tokens. It operates character-by-character using pointer arithmetic on `PAnsiChar`.

## State

```pascal
TScanner = record
  start:   pAnsiChar;   // beginning of current lexeme
  current: pAnsiChar;   // current read position
  line:    integer;      // current line number (for error reporting)
end;
```

Initialized by `InitScanner(source)`, which sets both `start` and `current` to the beginning of the source buffer. The source must be null-terminated (`#0`).

## Token Representation

```pascal
TToken = record
  tokenType: TTokenType;
  start:     pAnsiChar;   // pointer into source buffer (no copy)
  length:    integer;      // lexeme length
  line:      integer;
end;
```

Tokens are lightweight — they don't own their text. They point directly into the source buffer. This means the source must remain alive for the duration of compilation.

## Token Types

40 token types covering:
- **Single-character**: `( ) { } , . - + ; / * %`
- **One or two character**: `! != = == > >= < <=`
- **Literals**: `TOKEN_IDENTIFIER`, `TOKEN_STRING`, `TOKEN_NUMBER`
- **Keywords**: `and`, `class`, `else`, `false`, `for`, `fun`, `if`, `nil`, `or`, `print`, `return`, `super`, `this`, `true`, `var`, `while`, `record`
- **Synthetic**: `TOKEN_ERROR`, `TOKEN_EOF`

## Core Loop: `ScanToken`

```pascal
function ScanToken: TToken;
```

1. Skip whitespace and comments (`SkipWhitespace`)
2. Set `Scanner.start := Scanner.current`
3. Check for EOF → `TOKEN_EOF`
4. Advance one character
5. Dispatch:
   - `isAlpha(c)` → `Identifier` (may resolve to keyword)
   - `isDigit(c)` → `ScanNumber`
   - Single char (`(`, `)`, etc.) → `MakeToken`
   - Two-char operators (`!`, `=`, `<`, `>`) → check for `=` follow
   - `"` → `ScanString`
   - Otherwise → `ErrorToken('Unexpected character.')`

## Whitespace & Comments

`SkipWhitespace` consumes spaces, tabs, carriage returns, newlines (incrementing `line`), and line comments (`//` to end of line). Block comments are not supported.

## Keyword Recognition

Keywords are identified by a trie-like `identifierType` function that switches on the first character, then calls `CheckKeyword` to compare the remaining characters:

```pascal
function CheckKeyword(start, length: Integer; const rest: pAnsiChar;
  tokenType: TTokenType): TTokenType;
```

This avoids a hash lookup for every identifier — it's a hand-written trie with at most 3 levels of branching. Notably, `record` and `return` share the prefix `re` and branch on the third character (`c` vs `t`).

## String Scanning

```pascal
function ScanString: TToken;
```

Consumes characters until a closing `"` or EOF. Supports multi-line strings (increments `line` on `\n`). Returns `ErrorToken('Unterminated string.')` if EOF is reached first. No escape sequences are processed at the scanner level.

## Number Scanning

```pascal
function ScanNumber: TToken;
```

Consumes digits, then optionally a `.` followed by more digits. Does not handle scientific notation (`1e10`), hex (`0xFF`), or leading sign (that's handled by the parser as unary minus).

## Delphi-Specific Notes

- Uses `PAnsiChar` throughout (not `PWideChar`/`PChar`) for byte-level control
- `{$POINTERMATH ON}` enables `scanner.current[-1]` and pointer subtraction
- `advance` increments `current` and returns the previous character via `current[-1]`
- `peek` returns `current^` without advancing
- `peekNext` returns `(current + 1)^` with an EOF guard
- `match(expected)` is a conditional advance (consume if next char matches)
- `isAtEnd` checks `current^ = #0`

## Integration with the Parser

The parser calls `ScanToken` on demand via `AdvanceParser()`:

```pascal
procedure AdvanceParser;
begin
  Parser.Previous := Parser.Current;
  while true do
  begin
    parser.current := ScanToken;
    if parser.Current.tokenType <> TOKEN_ERROR then break;
    errorAtCurrent(Parser.Current.start);
  end;
end;
```

Error tokens are consumed in a loop, reporting each error, until a valid token is produced.
