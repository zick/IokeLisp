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

sym_t = makeSym("t")
sym_quote = makeSym("quote")

Error = Origin mimic
Error initialize = method(s, @data = s)

Cons = Origin mimic
Cons initialize = method(a, d,
  @car = a
  @cdr = d)
makeCons = method(a, d,
  Cons mimic(a, d))

nreverse = method(lst,
  ret = kNil
  while(lst kind?("Cons"),
    tmp = lst cdr
    lst cdr = ret
    ret = lst
    lst = tmp)
  ret)

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
      readList(str[1...str length]),
    str[0] == kQuote,
      tmp = read(str[1...str length])
      [makeCons(sym_quote, makeCons(tmp[0], kNil)), tmp[1]],
    true,
      readAtom(str)))

readList = method(str,
  ret = kNil
  loop(
    str = skipSpaces(str)
    if(str length == 0,
      return [Error mimic("unfinished parenthesis"), ""])
    if(str[0] == kRPar,
      break)
    tmp = read(str)
    if(tmp[0] kind?("Error"),
      return tmp)
    ret = makeCons(tmp[0], ret)
    str = tmp[1])
  [nreverse(ret), str[1...str length]])

printObj = method(obj,
  cond(
    obj kind?("Nil"), "nil",
    obj kind?("Num"), obj data asText,
    obj kind?("Sym"), obj data,
    obj kind?("Error"), "<error: " + obj data + ">",
    obj kind?("Cons"), printList(obj),
    obj kind?("Subr"), "<subr>",
    obj kind?("Expr"), "<expr>",
    true, "<unknown>"))

printList = method(obj,
  ret = ""
  first = true
  while(obj kind?("Cons"),
    if(first,
      first = false,
      ret += " ")
    ret += printObj(obj car)
    obj = obj cdr)
  if(obj == kNil,
    "(" + ret + ")",
    "(" + ret + " . " + printObj(obj) + ")"))

findVar = method(sym, env,
  while(env kind?("Cons"),
    alist = env car
    while(alist kind?("Cons"),
      if(alist car car == sym,
        return alist car)
      alist = alist cdr)
    env = env cdr)
  kNil)

g_env = makeCons(kNil, kNil)

addToEnv = method(sym, val, env,
  env car = makeCons(makeCons(sym, val), env car))

eval1 = method(obj, env,
  if(obj kind?("Nil") || obj kind?("Num") || obj kind?("Error"),
    return obj)
  if(obj kind?("Sym"),
    bind = findVar(obj, env)
    if(bind == kNil,
      return Error mimic(obj data + " has no value"),
      return bind cdr))
  Error mimic("noimpl"))

addToEnv(sym_t, sym_t, g_env)

ireader = java:io:InputStreamReader new(java:lang:System field:in)
breader = java:io:BufferedReader new(ireader)
loop(
  "> " print
  line = breader readLine
  if(line == nil,
    break)
  line = line asText  ; Covert java.lang.String -> Text.
  printObj(eval1(read(line)[0], g_env)) println)
