kLPar = 0x28
kRPar = 0x29
kQuote = 0x27

Nil = Origin mimic
kNil = Nil mimic

Num = Origin mimic
Num initialize = method(n, @data = n)

Sym = Origin mimic
Sym initialize = method(s, @data = s)

symTable = {"nil" => kNil}
makeSym = method(s,
  if(!symTable key?(s),
    symTable[s] = Sym mimic(s))
  symTable[s])

Error = Origin mimic
Error initialize = method(s, @data = s)

Cons = Origin mimic
Cons initialize = method(a, d,
  @car = a
  @cdr = d)

isSpace = method(c,
  c == 0x09 || c == 0x0a || c == 0x0d || c == 0x20)

isDelimiter = method(c,
  c == kLPar || c == kRPar || c == kQuote || isSpace(c))

skipSpaces = method(str,
  i = 0
  while(i < str length,
    if(!isSpace(str[i]),
      break)
    i++)
  str[i...str length])

isNumChar = method(c,
  0x30 <= c && c <= 0x39)  ; '0' <= c <= '9'

toNum = method(c,
  c - 0x30)  ; c - '0'

makeNumOrSym = method(str,
  i = 0
  sign = 1
  if(str[0] == 0x2d,  ; '-'
    sign = -1
    i = 1)
  is_num = false
  num = 0
  while(i < str length,
    if(isNumChar(str[i]),
      ;; then
      is_num = true
      num = num * 10 + toNum(str[i]),
      ;; else
      is_num = false
      break)
    i++)
  if(is_num,
    Num mimic(num * sign),
    makeSym(str)))

readAtom = method(str,
  next = ""
  i = 0
  while(i < str length,
    if(isDelimiter(str[i]),
      next = str[i...str length]
      str = str[0...i]
      break)
    i++)
  [makeNumOrSym(str), next])

read = method(str,
  str = skipSpaces(str)
  cond(
    str length == 0,
      [Error mimic("empty input"), ""],
    str[0] == kRPar,
      [Error mimic("invalid syntax: " + str), ""],
    str[0] == kLPar,
      [Error mimic("noimpl"), ""],
    str[0] == kQuote,
      [Error mimic("noimpl"), ""],
    true,
      readAtom(str)))

ireader = java:io:InputStreamReader new(java:lang:System field:in)
breader = java:io:BufferedReader new(ireader)
loop(
  "> " print
  line = breader readLine
  if(line == nil,
    break)
  line = line asText  ; Covert java.lang.String -> Text.
  read(line)[0] data println)
