/* File miniooYACC.mly */

%{ (* header *)
exception Fail of string;;
type varAttr = string list option;;

type cmdType =
  Decl of string * cmdType * varAttr
| SeqCtl  of cmdType * cmdType * varAttr
| Par  of cmdType * cmdType * varAttr
| If  of boolType * cmdType * cmdType * varAttr
| While  of boolType  * cmdType * varAttr
| Rpc of exprType * exprType * varAttr
| Dyn of string * varAttr
| Varassign of string * exprType * varAttr
| Fieldassign of exprType * exprType * exprType * varAttr
| Atom of cmdType * varAttr
| Block of cmdType
| Skip
| Empty

and exprType =
  Field of string
| Var of string * varAttr
| Number of int
| Locex of exprType * exprType * varAttr
| Subtract of exprType * exprType * varAttr
| Plus of exprType * exprType * varAttr
| Mult of exprType * exprType * varAttr
| Proc of string * cmdType * varAttr
| Null



and boolType =
  True
| False
| Eq of exprType * exprType * varAttr
| Lt of exprType * exprType * varAttr
;;

let rec print_indent num =
    match num with
    0 -> print_string " "
  | i -> print_string " "; print_indent (i-1)
  ;;

let rec print_ast_tree cmd indent  =
  match cmd with

    Decl(s, c, _) ->
      print_indent indent; print_string ("Declaration:"^s) ;print_newline();
      print_indent indent; print_string "Command List:" ;print_newline();
      print_ast_tree c  (indent+5);print_newline();
  | Block c ->
      print_indent indent; print_string("Command block:");print_newline();
      print_ast_tree c (indent+5);
  | Par(c1,c2,_) ->
      print_indent indent; print_string("Parallel Commandblock:");print_newline();
      print_ast_tree c1 (indent+5);print_newline();
      print_ast_tree c2 (indent+5);print_newline();
  | Atom(c,_) ->
      print_indent indent; print_string("Atom block:");print_newline();
      print_ast_tree c (indent+5);
  | Rpc(e1, e2, _) ->
      print_indent indent; print_string("Remote procedure call block:");print_newline();
      print_expr e1 (indent+5);print_newline();
      print_expr e2 (indent+5);print_newline();
  | Dyn(s, _) ->
      print_indent  indent; print_string("Dynamic memory allocation: ");print_newline();
      print_indent  (indent+5); print_string("Malloc:");print_newline();
      print_indent  (indent+5); print_string("Identifier:"^s);print_newline();
  | Varassign(s, e, _) ->
      print_indent indent; print_string("Variable Assignment:");print_newline();
      print_indent (indent+5); print_string("Identifier:"^s);print_newline();
      print_expr e (indent+5);
  | SeqCtl(c1, c2, _) ->
      print_indent indent; print_string("Sequential control:");print_newline();
      print_indent indent; print_string("Command list 1:");print_newline();print_ast_tree c1 (indent+5);print_newline();
      print_indent indent; print_string("Command list 2:");print_newline();print_ast_tree c2 (indent+5);print_newline();
  | If(b,c1,c2,_) ->
      print_indent indent; print_string("If Block:");print_newline();
      print_bool b (indent+5);print_newline();print_ast_tree c1 (indent+5);print_newline();
      print_indent indent; print_string("Else Block:");print_newline();
      print_ast_tree c2 (indent+5);
  | While(b,c,_) ->
      print_indent indent; print_string("While Block:");print_newline();
      print_bool b (indent+5);print_newline();
      print_ast_tree c (indent+5);
  | Fieldassign(e1, e2, e3, _) ->
      print_indent indent; print_string("Field Assignment:");print_newline();
      print_expr e1 (indent+5);print_newline();
      print_indent (indent+5); print_string("Operator .");print_newline();
      print_expr e2 (indent+5);print_newline();
      print_indent (indent+5); print_string("Operator =");print_newline();
      print_expr e3 (indent+5);print_newline();

  | Empty -> print_string " empty"
  | Skip -> print_indent indent; print_string("SKIP ");print_newline();

and print_expr expr indent=
  match expr with
    Field s ->
      print_indent indent;print_string("Expression :"   );print_newline();
      print_indent (indent+5); print_string ("Field ="^s);

  | Number i ->
      print_indent indent ; print_string("Expression: ");
      print_newline();
      print_indent (indent+5); print_string("Numeric literal "^(string_of_int i));
  | Var (s,_) ->
      print_indent indent ; print_string("Expression: ");
      print_newline();
      print_indent (indent+5); print_string("Identifier  "^(s));
  | Null ->
      print_indent indent ; print_string("Expression: ");
      print_newline();
      print_indent (indent+5); print_string("Null  ");
  | Locex(e1,e2,_) ->
      print_indent indent ; print_string("Location expression: ");print_newline();
      print_expr e1 (indent+5); print_newline();
      print_expr e2 (indent+5); print_newline();
  | Subtract(e1, e2, _) ->
      print_indent indent ; print_string("Arithmetic Expression: ");print_newline();
      print_expr e1 (indent+5); print_newline();
      print_indent (indent+5) ; print_string("Operator: - ");print_newline();
      print_expr e2 (indent+5); print_newline();
  | Plus(e1, e2, _) ->
      print_indent indent ; print_string("Arithmetic Expression: ");print_newline();
      print_expr e1 (indent+5); print_newline();
      print_indent (indent+5) ; print_string("Operator: + ");print_newline();
      print_expr e2 (indent+5); print_newline();
  | Mult(e1, e2, _) ->
      print_indent indent ; print_string("Arithmetic Expression: ");print_newline();
      print_expr e1 (indent+5); print_newline();
      print_indent (indent+5) ; print_string("Operator: * ");print_newline();
      print_expr e2 (indent+5); print_newline();
  | Proc(s,c,_) ->
      print_indent indent; print_string("Recursive procedure :"^s);print_newline();
      print_ast_tree c (indent+5);

and print_bool bool indent =
  match bool with
    True ->
      print_indent indent;print_string("Boolean Expression ");print_newline();
      print_indent indent;print_string("Literal->True");

  | False ->
      print_indent indent;print_string("Boolean Expression ");print_newline();
      print_newline();print_string("Literal->False");
  | Eq(e1, e2, _) ->
      print_indent indent;print_string("Boolean Expression ");print_newline();
      print_expr e1 (indent+5);print_newline();
      print_indent (indent+5);print_string("Boolean Operator : == ");print_newline();
      print_expr e2 (indent+5);print_newline();

  | Lt(e1, e2, _) ->
      print_indent indent;print_string("Boolean Expression ");print_newline();
      print_expr e1 (indent+5);print_newline();
      print_indent (indent+5);print_string("Boolean Operator : <");print_newline();
      print_expr e2 (indent+5);print_newline();
  ;;

(* Expand is a function which expands a symbol table with a variable v*)
let expand s v =
    match v with
      Some l -> Some (s::l)
      | _ -> raise (Fail "expand: the v is nothing")
    ;;

(* set_cmd is a function with sets attributes for a cmd in the vattr field *)
(* set_expr is a function with sets attributes for an expr in the vattr field*)
(* set_expr is a function with sets attributes for a bool in the vattr field*)
let rec set_cmd cmd v =
  match cmd with
    Decl(s, c, _) ->
              let newList = expand s v in
              Decl(s,set_cmd c newList , v)
  | Varassign(s, e, _) -> Varassign(s,set_exp e v, v)
  | Dyn(s,_) -> Dyn(s,v)
  | Par(c1,c2,_) -> Par(set_cmd c1 v, set_cmd c2 v, v)
  | Fieldassign(e1,e2,e3,_) -> Fieldassign (set_exp e1 v, set_exp e2 v , set_exp e3 v, v)
  | Skip -> Skip
  | SeqCtl(c1,c2,_) -> SeqCtl(set_cmd c1 v , set_cmd c2 v, v)
  | If(b,c1,c2,_) -> If(set_bool b v, set_cmd c1 v, set_cmd c2 v, v)
  | While(b,c1,_) -> While(set_bool b v, set_cmd c1 v, v)
  | Atom(c1,_) -> Atom(set_cmd c1 v,v)
  | Rpc(e1,e2,_) -> Rpc(set_exp e1 v , set_exp e2 v, v)
  | _ -> raise (Fail "set_cmd: invalid pattern in static check stage")

  and set_exp expr v =
    match expr with
    Field s -> Field s
  | Number i -> Number i
  | Var (s,_) -> Var (s, v)
  | Null -> Null
  | Mult(e1, e2, _) -> Mult(set_exp e1 v, set_exp  e2 v, v)
  | Subtract(e1, e2, _) -> Subtract(set_exp e1 v, set_exp  e2 v, v)
  | Plus(e1, e2, _) -> Plus(set_exp e1 v, set_exp  e2 v, v)
  | Locex(e1,e2,_) -> Locex(set_exp e1 v, set_exp e2 v, v)
  | Proc(s,c,_) ->
      let newList = expand s v in
     Proc(s, set_cmd c newList , v)
  and set_bool expr v =
    match expr with
      True -> True
    | False -> False
    | Eq(e1,e2,_) -> Eq (set_exp e1 v, set_exp e2 v, v)
    | Lt(e1,e2,_) -> Lt (set_exp e1 v, set_exp e2 v , v)
  ;;

  (* Search symbol table is a function which returns true if a variable is present in symbol table *)
  let rec search_symbol_table v e =
    match v with
    | Some list ->
        (
        match list with
        [] -> true;
        | h::t -> if h=e then false else search_symbol_table (Some t) e
        )
     | None -> raise (Fail "Not present in symbol table")
  ;;

  let rec check_cmd cmd =
      match cmd with
    Decl(s, c, v) -> check_cmd c
  | Varassign(s, e, v) -> search_symbol_table v s ||  check_exp e;
  | Rpc(e1,e2,v) -> check_exp e1 ||  check_exp e2
  | Dyn(s,v) ->  search_symbol_table v s
  | Fieldassign(e1,e2,e3,v) -> check_exp e1 || check_exp e2 || check_exp e3
  | Skip -> false
  | SeqCtl(c1,c2,v) -> check_cmd c1  || check_cmd c2
  | Par(c1,c2,v) -> check_cmd c1 ||  check_cmd c2
  | While(b,c1,v) -> check_bool b || check_cmd c1
  | If(b,c1,c2,v) -> check_bool b || check_cmd c1 || check_cmd c2
  | Atom(c1,v) -> check_cmd c1
  | _ -> raise (Fail "set_cmd: invalid pattern in static check stage")

  and  check_exp  expr =
    match expr with
    Field s -> false
  | Number i -> false
  | Var (s,v) ->  search_symbol_table v s
  | Null -> false
  | Subtract(e1, e2, _) -> check_exp e1 || check_exp e2
  | Plus(e1, e2, _) -> check_exp e1 || check_exp e2
  | Mult(e1, e2, _) -> check_exp e1 || check_exp e2
  | Locex(e1,e2,_) -> check_exp e1 || check_exp e2
  | Proc(_,c,_) -> check_cmd c

  and check_bool expr =
  match expr with
     True -> false
   | False -> false
   | Eq(e1,e2,_) -> check_exp e1 || check_exp e2
   | Lt(e1,e2,_) -> check_exp e1 || check_exp e2
  ;;
   (* Scope check is a function which first sets the symbol table for each command and then checks the symbol table for
   each expression within the command *)
    let  scope_check cmd =
        let result=  set_cmd cmd (Some[]) in check_cmd result;
      ;;

    let static_check prog =
      let result = scope_check prog in
      print_newline ();
      print_string "....Static Semantic Checking Start...";
      print_newline ();
      if result then print_string "Semantic Error..Check semantics" else print_string "Program passed the semantic test!";
      print_newline ();
      print_newline ()
      ;;

  type booleans = BoolTrue | BoolFalse | BoolError;;
  type objects = Obj of int  ;;
  type locations =
          Loc of objects
        | Lnull;;
  type var = V of string;;
  type field = F of string | Val ;;
  type env = Env of var * objects ;;
  type frame = Declare of env | Call of env * stack
      and  stack = frame list ;;
  type closure= Closure of var * cmdType * stack ;;
  type values = VField of field | VInt of int | VLoc of locations | Clo of closure;;
  type taintv = Value of values  | TvError ;;

(* Initialize Heap *)
type heap = ListEntry of  (field * taintv ref) list ref;;
let initialize_heap() = ref ([] : heap list);;
let initialize_stack() =  ([]: stack );;
type conf = Conf of cmdType * stack  * (heap list) ref
(* get location  *)
(*  getLoc:  heap ref -> int *)
let getLoc heap =
    let len= (List.length !heap)
        in Obj len;;



(* push a new frame into stack *)
(*  push:  stack -> stack *)
let rec push stack frame =
    match stack with
        [] -> [frame]
      | h::t ->  [frame] @ [h::t];;



(* pop the most recent frame from  stack *)
(* pop: stack -> unit *)
let pop stack =
  match stack with
        [] -> raise( Fail " looks like an empty stack")
  | h::t -> t
  ;;


  (* search a variable in stack and return location from heap string->stack->objects *)
  (* search :  stack -> string -> objects *)
  let rec search stack x =
    let var_exists env x =
        match env with
          Env(v,o) -> (
              match v with
              V var -> (if var = x then true else false) ) in
        let getobj env =
                match env with
                Env(_,o) ->  o   in

        match stack with
            [] -> raise (Fail "Variable not found" )
        | h::t -> (
            match h with
            Declare(env)  -> if  var_exists env x then begin getobj env end else search t x
          | Call(env,_) -> if var_exists env x then begin getobj env  end else search t x
        ) ;;

(* Get Field from a Tainted Value *)
(* getfield : taintv -> field *)
let getfield tvf =
  match tvf with
    Value v -> (
    match v with
        VField vf->vf
      | _ -> raise(Fail " Must be a field")
    )
   | _ -> raise(Fail " Must be a value")
  ;;

(* Get field value from a particular location on the heap *)
(* get_field_value : object-> field -> heap ref -> value *)
let get_field_value loc field heap  =
    let rec search_list_entry le =
      match le with
        hd::tl ->  (
          match hd with
          (rf,rv) -> if rf=field then !rv else search_list_entry tl
        )
      | [] -> Value(VLoc(Lnull)) in

    match loc with
      Loc newloc -> (
          match newloc with
              Obj n ->
                  if n>=List.length !heap then raise (Fail "List length exceeded in get_field_value. Invalid field location")
                  else
                      let entry = List.nth !heap n in
                      (match entry with
                          ListEntry le -> (
                            match  search_list_entry !le  with
                            Value v -> v
                          | _ -> raise (Fail "Not a valid field,value pair")
                          )

                      )
      )
    | Lnull -> raise (Fail "Null location ")

(* Get Value from a particular location in the heap *)
(* getvalue: object -> heap ref -> value *)

let getvalue loc heap  =
  match loc with
    Obj n ->
      if n >= List.length !heap then raise (Fail "List exceeded in getvalue loc heap")
      else
           let entry = List.nth !heap (n) in
           ( match entry with
              ListEntry le ->  (
                match !le with
                    hd::tl ->  (
                      match hd with
                      (field_ref,tval_ref) -> (
                        match field_ref with
                          Val -> !tval_ref
                        | _ -> raise (Fail "field is not val")
                        )
                      )
                  | [] -> raise (Fail "Empty (field,val) pairs")
                  )
                )
;;

(* Sets value for a particular field in heap*)
(* setvalue_in_heap: object -> field->value->heap ref -> bool *)

let  setvalue_in_heap loc field value heap  =
  let rec set_inner_list fl value l =
      match l with
        h::t -> (match h with
            (field_f,val_ref) -> if field_f=fl then begin val_ref:=value ;true end else set_inner_list fl value t )
        | [] -> false
  in
   match loc with
     Obj n ->
         if n >= List.length !heap then raise (Fail "List exceeded in setvalue_in_heap")
         else
              let entry = List.nth !heap n in
              (
                match entry with
                ListEntry h -> if set_inner_list field value !h then () else  h:= !h  @ [(field, ref value)]
              )
  ;;

(* Evaluates an expression *)
(* eval: exprType -> stack -> heap ref -> value*)

let rec eval expr stack heap =
      match expr with
        Field f -> print_string("new field");Value(VField(F f))
      | Number n -> Value(VInt(n))
      | Subtract(e1,e2,_) ->
            let tv1= eval e1 stack heap in
            let tv2= eval e2 stack heap in
            ( match tv1,tv2 with
              Value v1,Value v2 ->
                  (match v1,v2 with
                  VInt i1, VInt i2 -> Value(VInt(i1-i2))
                  | _,_ ->TvError
                  )
            | _,_ -> TvError
            )
      | Mult(e1,e2,_) ->
            let tv1= eval e1 stack heap in
            let tv2= eval e2 stack heap in
            ( match tv1,tv2 with
              Value v1,Value v2 ->
                  (match v1,v2 with
                  VInt i1, VInt i2 -> Value(VInt(i1*i2))
                  | _,_ ->TvError
                  )
            | _,_ -> TvError
            )
      | Plus(e1,e2,_) ->
            let tv1= eval e1 stack heap in
            let tv2= eval e2 stack heap in
            ( match tv1,tv2 with
              Value v1,Value v2 ->
                  (match v1,v2 with
                  VInt i1, VInt i2 -> Value(VInt(i1+i2))
                  | _,_ ->TvError
                  )
            | _,_ -> TvError
            )
      | Null -> Value(VLoc(Lnull))
      | Var(x,_) -> let loc= search stack x in
                     getvalue loc heap

      | Locex(e1,e2,_) ->
            let l = eval e1 stack heap in
            let f = eval e2 stack heap in
            (
            match l, f with
            Value v1, Value v2 ->
                ( match v1,v2 with
                  VLoc l1, VField f2 ->  Value(get_field_value l1 f2 heap)
                 | _,_ -> TvError
                )
            | _,_ -> TvError
            )
      | Proc(x,c,_) -> Value(Clo(Closure(V x,c,stack)))

      ;;

(*Evaluates a boolean expression *)
(* eval_bool: boolType -> stack -> heap ref -> boolean *)

let rec eval_bool expr stack heap  =
    match expr with
        True -> BoolTrue
      | False -> BoolFalse
      | Eq(b1,b2,_ ) ->
                    let tv1 = eval b1 stack heap in
                    let tv2 = eval b2 stack heap in
                    (match tv1,tv2 with
                        Value v1, Value v2  -> (
                          match v1, v2 with
                            VInt v1, VInt v2 -> if v1=v2 then BoolTrue else BoolFalse
                          | VLoc v1, VLoc v2 -> if v1=v2 then BoolTrue else BoolFalse
                          | Clo v1, Clo v2 ->if v1=v2 then BoolTrue else BoolFalse
                          | _ ,_ -> BoolError
                        )
                      | _,_ -> BoolError

                    )
      | Lt(b1,b2,_) ->
                    let tv1 = eval b1 stack heap in
                    let tv2 = eval b2 stack heap in
                    (match tv1,tv2 with
                        Value v1, Value v2  -> (
                          match v1, v2 with
                            VInt v1, VInt v2 -> if v1<v2 then begin print_string("true");BoolTrue end else BoolFalse
                          | _ ,_ -> print_string("Bool error in lt");BoolError
                        )
                      | _,_ -> BoolError

                    )
    ;;

let not_in_loc heap =
    let len= (List.length !heap)
        in Obj len;;
(* getconf is a function which takes a current configuration and returns the next configuration *)
(* getconf: conf -> conf *)

let rec getconf curr_conf =
    match curr_conf with
     Conf(c,stack,heap) -> (
      match c with
            Empty -> Conf(Empty, stack, heap)
            | Block cmd ->
                let next = getconf (Conf(cmd,stack,heap)) in
                ( match next with
                  Conf(Empty, stack, heap) ->
                    ( match stack with
                      [] -> raise (Fail "runtime error: no decl matched with a block, stack empty")
                    | h :: t ->
                        ( match h with
                          Declare _ ->
                            Conf(Empty, pop stack, heap)
                        | Call(_, s) ->

                            Conf(Empty, s, heap)
                        ))
                | Conf(c, stack, heap) ->Conf(Block  c, stack, heap) )
          | Decl(var, cmd, _) ->
            let newloc= getLoc heap in
            let myframe = Declare(Env(V var,newloc)) in
            heap := !heap @  [ListEntry (ref [( Val, ref (Value(VLoc(Lnull))) )])]  ;
            print_string("heap location created for " ^var);print_newline();
            Conf(Block cmd, myframe::stack, heap)
          | SeqCtl(cmd1, cmd2, attr) ->
              let next= getconf (Conf(cmd1, stack, heap))
                in (
                match next with
                Conf(nextcmd, newstack,newheap) -> (
                match nextcmd with
                Empty ->Conf(cmd2, stack, heap)
              | cmd-> Conf(SeqCtl(cmd,cmd2,attr), newstack, newheap)
              )
              )
          | Par(cmd1, cmd2, attr) ->
              (match Random.bool() with
              true -> (
                let next=getconf (Conf(cmd1,stack,heap)) in (
                match next with
                  Conf(nextcmd,newstack,newheap)-> (
                  match nextcmd with
                    Empty -> Conf(cmd2,stack,heap)
                  | somecmd -> Conf(Par(somecmd,cmd2,attr),newstack,newheap)
                  )
                )
              )
              | false -> (
                let next=getconf (Conf(cmd2,stack,heap)) in (
                match next with
                Conf(nextcmd,newstack,newheap)-> (
                    match nextcmd with
                    Empty -> Conf(cmd1,stack,heap)
                    | somecmd -> Conf(Par(cmd1,somecmd,attr),newstack,newheap)
                    )
                    )
                )
                )

          | If(b,cmd1,cmd2,_)  ->
                let b1= eval_bool b stack heap in
                (match b1 with
                   BoolTrue -> Conf(cmd1, stack, heap)
                 | BoolFalse -> Conf(cmd2, stack, heap)
                 | BoolError -> raise (Fail "Error in boolean expression" ))
          | While(b,cmd1,varAttr) ->
                let b1 = eval_bool  b stack heap in
                (match b1 with
                  BoolTrue ->  print_string("Bool true");Conf(SeqCtl(cmd1,While(b,cmd1,varAttr),varAttr),stack,heap)
                | BoolFalse -> print_string("Bool false");Conf(Empty,stack,heap)
                | BoolError -> print_string("Bool error");raise (Fail " Boolean expression error")
                )

          | Skip -> Conf(Empty,stack,heap)
          | Atom(cmd,at) ->(
              let next =  getconf (Conf(cmd, stack, heap))
              in  match next with
                Conf(somecmd,newstack,newheap) -> (
                    match somecmd with
                      Empty -> Conf(Empty,stack,heap)
                    | newcmd -> getconf (Conf(Atom(newcmd,at),newstack,newheap))
              )
          )
          | Rpc (e1, e2, _) ->
              let v = eval e1 stack heap in (
              match v with
                  Value va  -> (
                    match va with
                        Clo clo -> (
                          match clo with
                          Closure(var,c,stack1) ->
                          let newloc=not_in_loc heap in
                          let stack2 = Call(Env(var,newloc),stack) :: stack1 in
                          let newval= eval e2 stack heap in
                          heap:= !heap @ [ListEntry(ref [])];
                          setvalue_in_heap newloc Val newval heap;
                          print_string("before calling block of RPC");
                          print_newline();
                          Conf(Block c,stack2,heap)

                        )
                      | _ -> raise (Fail"not a closure")
                    )
                | _ -> raise (Fail "Not a value")
              )
          | Dyn (var, _) ->
              let newloc = not_in_loc heap in
              let vloc = Value(VLoc(Loc newloc)) in
              let l = search  stack  var in
              heap := !heap @ [ListEntry (ref [])];
              setvalue_in_heap l Val vloc heap;
              Conf(Empty,stack,heap)
          | Varassign(x, e, _) ->
              let v = eval e stack heap in
                if v = TvError
                  then
                    raise (Fail "runtime error - Variable assignment")
                else
                  let l = search stack x in
                  setvalue_in_heap l Val v heap ;
              Conf(Empty,stack,heap)
          | Fieldassign (e1, e2, e3, _) ->
          let l = eval e1 stack heap in
          let f = eval e2 stack heap in
          let v = eval e3 stack heap in
            let obj =
              ( match l with
                Value tv ->
                  ( match tv with
                    VLoc loc ->
                      ( match loc with
                        Loc o -> o
                      | Lnull -> raise (Fail "Looks like the variable is not malloc'ed yet")
                    )
                | _ -> raise (Fail " runtime error - not matched with a VLoc. Error in your Field assignment statemnt") )
              | _ -> raise (Fail "runtime error not matched with a value. Error in your Field assignment statement") ) in
            let sf = getfield f in
            setvalue_in_heap obj sf v heap;
            Conf(Empty,stack,heap)
          )
  ;;


    (* print_all_stack prints all the frames in the stack *)
    (* print_all_stack : stack -> unit *)

    let print_stack count indent stack =
      let print_env env =
          match env with
            Env (var, obj) ->
              ( match var, obj with
                V s, Obj i ->
                  print_string "(";
                  print_string s;
                  print_string " , ";
                  print_string (string_of_int i); print_string ")" ) in

      let rec print_all_stack  count indent stack =
        match stack with
          [] -> ();
        | h :: t ->
            print_indent indent;
            print_string "[";
            print_string (string_of_int count);
            print_string "] ";
            print_frame indent h;
            print_newline ();
            print_all_stack  (count + 1)  indent t
      and print_frame indent frame =
        match frame with
          Declare env ->
            print_string "Declare ";
            print_env env;
        | Call (env, stack) ->
            print_string "Call ";
            print_env env;
            print_newline ();
            print_all_stack  0 (indent+10) stack in

      print_all_stack  0 indent stack ;;


(* print_heap prints all the heap objects in the heap *)
(* print_heap :  heap ref -> unit *)


      let rec print_heap  heap=
            let print_field fv =
            match fv with
               F fv -> print_string(fv)
            | Val -> print_newline();print_string("val") in

            let print_location lv =
                match lv with
                Lnull -> print_string("lnull")
              | Loc i -> (
              match i with
                Obj n -> print_string("Heap [");
                          print_string(string_of_int(n));
                          print_string("]");

              )  in
            let print_closure clo =
              let print_var var =
                match var with
                  V s -> print_string s in

              match clo with
                Closure (var, cmd, stack) ->
                  print_string "closure of  ";
                  print_var var;

                  (*print_stack 0 0 stack*)
                  in

            let print_tvalue tv =
            match tv with
              TvError -> print_string("tainted value error")
            | Value v -> (
            match v with
                 VField(f) -> print_field(f)
                | VInt i ->  print_string(string_of_int i) ;
                | VLoc l-> print_location l;
                | Clo c-> print_closure c;
            ) in

            let rec print_inner_list list=
                (match list with
                  [] -> ()
                 | hd::tl ->
                  match hd with
                  field,value ->

                                 print_field field ;
                                 print_string("=");
                                 print_tvalue  !value;
                                 print_newline();
                                 print_inner_list tl ; ) in

       let rec print_all_heap count heaplist =
        (match heaplist with
                 [] -> ()
               | hd::tl ->  (
                 match hd with
                   ListEntry hdref ->
                         print_string "Heap[";
                         print_string (string_of_int count);
                         print_string "]";
                         print_inner_list !hdref ;
                         print_all_heap (count+1) tl;

               )
        ) in

           print_all_heap 0 heap ;;


(* calculate is a function which takes a configuration and returns a configuration *)
(* calculate : conf -> conf *)
let rec calculate conf  =
      let next_conf = getconf conf   in
      match next_conf with
          Conf(prog,stack,heap)   ->(
          match prog with
          Empty -> (
          print_newline ();
          print_string "-------------------- Printing Stack  --------------------";
          print_newline ();
          print_stack 0 0  stack;
          print_string "-------------------- Printing Heap contents--------------";
          print_newline ();
          print_heap  !heap;
          print_newline ();

          print_newline ();
          print_newline ();
          print_string " Terminated without error ";
          print_newline ();
          print_newline ();
          print_newline (); )
        | nextcmd ->  (

        print_newline();
        print_string "-------------------- Stack  ";
        print_newline ();
        print_stack 0 0 stack;
        print_string "-------------------- Heap   ";
        print_newline();
        print_heap  !heap;

        print_newline();
        calculate next_conf
        )
      )
  ;;

(* interpret is the main function of miniOO interpreter: It takes a program and returns a unit *)
(* interpret: prog -> unit *)

let interpret prog =
      print_string("-------------Starting interpreter ------------") ;
      print_newline();
      let heap = initialize_heap() in
      let stack = initialize_stack() in
      calculate (Conf(prog,stack,heap))
  ;;

%} /* declarations */
%token EOL SEMICOLON ASSIGN PLUS /* lexer tokens */
%token MINUS TIMES DIV LPAREN RPAREN  LCURL RCURL IF ELSE
%token VARIABLE   PROC NULL COLON EQUALS MALLOC ATOM TRUE FALSE
%token WHILE EQUALS LT SKIP MULT PARALLEL
%token < string >  FIELD
%token < string > VAR
%token < int > NUMBER

%start prog                   /* the entry point */
%type <unit> prog
%type <boolType> bool
%type <cmdType> cmd
%type <exprType> expr
%left PLUS MINUS            /* lowest precedence */
%left TIMES DIV  MULT           /* medium precedence */


%% /* rules */

prog :
    cmd EOL  { print_ast_tree $1 5  ;static_check $1 ;  interpret $1 ; flush stdout; () }

cmd :
  | decl    {$1}
  | assign  { $1 }
  | seqctl  {$1}
  | recr {$1}
  | parr {$1}
  | dyn  {$1}
  | atom  {$1}
  | SKIP  { Skip}

parr :
  LCURL cmd PARALLEL cmd RCURL { Par($2,$4,None)}
dyn :
  MALLOC LPAREN VAR RPAREN {Dyn($3,None)}
recr:
   expr LPAREN expr RPAREN { Rpc($1, $3, None)  }
decl:
    VARIABLE VAR SEMICOLON cmd  { Decl($2, $4, None)  }
seqctl:
    LCURL cmd SEMICOLON cmd RCURL { SeqCtl($2, $4, None)  }
  | IF bool cmd ELSE cmd {  If($2,$3,$5,None)  }
  | WHILE bool cmd  {  While($2,$3,None)  }

assign :
    VAR ASSIGN expr  { Varassign($1, $3, None)   }
  | expr TIMES expr ASSIGN expr {Fieldassign($1,$3,$5,None) }

atom:
  ATOM LPAREN cmd RPAREN {Atom($3,None)}
expr :
    FIELD                    { Field  $1                 }
  | VAR                    { Var ($1,None) }
  | PROC VAR COLON cmd   { Proc($2,$4,None)}
  | expr TIMES expr  {Locex($1,$3,None)}
  | expr MINUS expr          { Subtract  ($1, $3, None)  }
  | expr PLUS expr          { Plus  ($1, $3, None)  }
  | expr MULT expr          { Mult  ($1, $3, None)  }
  | LPAREN expr RPAREN       { $2 }
  | NUMBER                   {Number $1 }

bool :
    TRUE          {True }
  | FALSE         {False}
  | expr EQUALS expr  { Eq($1,$3,None)}
  | expr LT expr  { Lt($1,$3,None )}
%% (* trailer *)
