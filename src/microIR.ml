type value =
  | VReg of string
  | VConst of int
  | VUndef

type expr =
  | EVal of value
  | EBinop of string * value * value
  | ELoad of value
  | EAddr of value * int
  | EIndex of value * value * int
  | EField of value * string
  | ECmp of string * value * value

type instr =
  | IAssign of string * expr
  | IStore of value * value
  | IAssume of expr
  | IAssert of expr
  | ICall of string option * value * value list

type terminator =
  | TJump of string
  | TBranch of expr * string * string
  | TSwitch of value * (int * string) list * string
  | TReturn of value option
  | TStop

type block = {
  label : string;
  body : instr list;
  term : terminator;
}

type func = {
  fname : string;
  entry : string;
  params : string list;
  ret : string option;
  blocks : block list;
}

type prog = func list

let string_of_value = function
  | VReg v -> v
  | VConst n -> string_of_int n
  | VUndef -> "undef"

let string_of_expr =
  let rec go = function
    | EVal v -> string_of_value v
    | EBinop (op, a, b) ->
        Printf.sprintf "(%s %s %s)" (string_of_value a) op (string_of_value b)
    | ELoad v -> Printf.sprintf "load(%s)" (string_of_value v)
    | EAddr (v, off) -> Printf.sprintf "addr(%s,%d)" (string_of_value v) off
    | EIndex (base, idx, width) ->
        Printf.sprintf "index(%s,%s,%d)" (string_of_value base) (string_of_value idx) width
    | EField (base, fld) ->
        Printf.sprintf "field(%s,%s)" (string_of_value base) fld
    | ECmp (pred, a, b) ->
        Printf.sprintf "(%s %s %s)" (string_of_value a) pred (string_of_value b)
  in
  go

let negate_expr = function
  | ECmp ("eq", a, b) -> ECmp ("ne", a, b)
  | ECmp ("ne", a, b) -> ECmp ("eq", a, b)
  | ECmp ("lt", a, b) -> ECmp ("ge", a, b)
  | ECmp ("le", a, b) -> ECmp ("gt", a, b)
  | ECmp ("gt", a, b) -> ECmp ("le", a, b)
  | ECmp ("ge", a, b) -> ECmp ("lt", a, b)
  | e -> e

let string_of_negated_expr e =
  match e with
  | ECmp _ -> string_of_expr (negate_expr e)
  | _ -> Printf.sprintf "!(%s)" (string_of_expr e)

let sample_block =
  {
    label = "entry";
    body = [IAssign ("r0", EVal (VConst 0))];
    term = TReturn (Some (VReg "r0"));
  }

let loop_example =
  let entry =
    {
      label = "entry";
      body = [IAssign ("i", EVal (VConst 0)); IAssign ("sum", EVal (VConst 0))];
      term = TJump "hdr";
    }
  in
  let hdr =
    {
      label = "hdr";
      body = [];
      term = TBranch (ECmp ("lt", VReg "i", VConst 10), "body", "exit");
    }
  in
  let body =
    {
      label = "body";
      body =
        [
          IAssign ("sum", EBinop ("add", VReg "sum", VReg "i"));
          IAssign ("i", EBinop ("add", VReg "i", VConst 1));
        ];
      term = TJump "hdr";
    }
  in
  let exit =
    { label = "exit"; body = []; term = TReturn (Some (VReg "sum")) }
  in
  {
    fname = "sum_to_ten";
    entry = "entry";
    params = [];
    ret = Some "sum";
    blocks = [ entry; hdr; body; exit ];
  }

let branch_example =
  let entry =
    {
      label = "entry";
      body = [];
      term = TBranch (ECmp ("eq", VReg "x", VConst 0), "then0", "else0");
    }
  in
  let then0 =
    { label = "then0"; body = [ IAssign ("r", EVal (VConst 1)) ]; term = TJump "join" }
  in
  let else0 =
    { label = "else0"; body = [ IAssign ("r", EVal (VConst 2)) ]; term = TJump "join" }
  in
  let join =
    { label = "join"; body = []; term = TReturn (Some (VReg "r")) }
  in
  {
    fname = "branch_only";
    entry = "entry";
    params = [ "x" ];
    ret = Some "r";
    blocks = [ entry; then0; else0; join ];
  }

let sample_prog =
  [
    { fname = "main"; entry = "entry"; params = []; ret = Some "r0"; blocks = [ sample_block ] };
    loop_example;
    branch_example;
  ]
