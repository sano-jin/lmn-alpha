
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | VBAR
    | RPAREN
    | RBRACKET
    | LinkName of (
# 9 "compiler/parser/parser.mly"
       (string)
# 14 "compiler/parser/parser.ml"
  )
    | LPAREN
    | LBRACKET
    | EQ
    | EOF
    | DOT
    | COMMA
    | COLMIN
    | AtomName of (
# 8 "compiler/parser/parser.mly"
       (string)
# 26 "compiler/parser/parser.ml"
  )
    | ATAT
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState45
  | MenhirState38
  | MenhirState36
  | MenhirState29
  | MenhirState26
  | MenhirState21
  | MenhirState15
  | MenhirState7
  | MenhirState5
  | MenhirState4
  | MenhirState3
  | MenhirState0

# 3 "compiler/parser/parser.mly"
  
  open Syntax

# 62 "compiler/parser/parser.ml"

let rec _menhir_goto_maybe_name_rule : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.proc) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (Syntax.proc)) = _v in
    let _v : (Syntax.proc) = 
# 117 "compiler/parser/parser.mly"
                    (_1)
# 72 "compiler/parser/parser.ml"
     in
    _menhir_goto_proc _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_block : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.proc) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_3 : (Syntax.proc)) = _v in
        let (_menhir_stack, _menhir_s, (_1 : (Syntax.proc))) = _menhir_stack in
        let _v : (Syntax.proc) = 
# 124 "compiler/parser/parser.mly"
                   ( Mol (_1, _3) )
# 87 "compiler/parser/parser.ml"
         in
        _menhir_goto_block _menhir_env _menhir_stack _menhir_s _v
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_1 : (Syntax.proc)) = _v in
        let _v : (Syntax.proc) = 
# 135 "compiler/parser/parser.mly"
          ( _1 )
# 97 "compiler/parser/parser.ml"
         in
        _menhir_goto_program _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_rule : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.proc * Syntax.proc) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_3 : (Syntax.proc * Syntax.proc)) = _v in
        let (_menhir_stack, _menhir_s, (_1 : (
# 8 "compiler/parser/parser.mly"
       (string)
# 113 "compiler/parser/parser.ml"
        ))) = _menhir_stack in
        let _v : (Syntax.proc) = 
# 97 "compiler/parser/parser.mly"
                       ( Rule (Some _1, _3))
# 118 "compiler/parser/parser.ml"
         in
        _menhir_goto_maybe_name_rule _menhir_env _menhir_stack _menhir_s _v
    | MenhirState45 | MenhirState0 | MenhirState3 | MenhirState38 | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_1 : (Syntax.proc * Syntax.proc)) = _v in
        let _v : (Syntax.proc) = 
# 98 "compiler/parser/parser.mly"
         ( Rule (None, _1))
# 128 "compiler/parser/parser.ml"
         in
        _menhir_goto_maybe_name_rule _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run29 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.proc) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AtomName _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | LBRACKET ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | LPAREN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | LinkName _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29

and _menhir_run38 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.proc) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AtomName _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | LBRACKET ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | LPAREN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | LinkName _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | COLMIN | COMMA | DOT | EOF | RPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (_1 : (Syntax.proc))) = _menhir_stack in
        let _v : (Syntax.proc * Syntax.proc) = 
# 93 "compiler/parser/parser.mly"
                ((_1, Zero))
# 171 "compiler/parser/parser.ml"
         in
        _menhir_goto_rule _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38

and _menhir_goto_proc : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.proc) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COLMIN ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (_1 : (Syntax.proc))), _, (_3 : (Syntax.proc))) = _menhir_stack in
        let _v : (Syntax.proc) = 
# 115 "compiler/parser/parser.mly"
                    ( Mol (_1, _3) )
# 205 "compiler/parser/parser.ml"
         in
        _menhir_goto_proc _menhir_env _menhir_stack _menhir_s _v
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | DOT | EOF | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Syntax.proc))), _, (_3 : (Syntax.proc))) = _menhir_stack in
            let _v : (Syntax.proc * Syntax.proc) = 
# 92 "compiler/parser/parser.mly"
                     ((_1, _3))
# 221 "compiler/parser/parser.ml"
             in
            _menhir_goto_rule _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COLMIN ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Syntax.proc))) = _menhir_stack in
            let _v : (Syntax.proc) = 
# 119 "compiler/parser/parser.mly"
                       ( _2 )
# 247 "compiler/parser/parser.ml"
             in
            _menhir_goto_proc _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState45 | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COLMIN ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | DOT ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | AtomName _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
            | LBRACKET ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | LPAREN ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | LinkName _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
            | EOF ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, (_1 : (Syntax.proc))) = _menhir_stack in
                let _v : (Syntax.proc) = 
# 125 "compiler/parser/parser.mly"
             ( _1 )
# 284 "compiler/parser/parser.ml"
                 in
                _menhir_goto_block _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45)
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (_1 : (Syntax.proc))) = _menhir_stack in
            let _v : (Syntax.proc) = 
# 126 "compiler/parser/parser.mly"
         ( _1 )
# 297 "compiler/parser/parser.ml"
             in
            _menhir_goto_block _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce2 : _menhir_env -> 'ttv_tail * _menhir_state * (Syntax.arg) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (_1 : (Syntax.arg))) = _menhir_stack in
    let _v : (Syntax.arg) = 
# 58 "compiler/parser/parser.mly"
         ( _1 )
# 315 "compiler/parser/parser.ml"
     in
    _menhir_goto_arg _menhir_env _menhir_stack _menhir_s _v

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_graph : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.arg) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (Syntax.arg)) = _v in
    let _v : (Syntax.proc) = 
# 113 "compiler/parser/parser.mly"
          ( Graph _1 )
# 332 "compiler/parser/parser.ml"
     in
    _menhir_goto_proc _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_args_inner : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.arg list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (
# 8 "compiler/parser/parser.mly"
       (string)
# 352 "compiler/parser/parser.ml"
            ))), _, (_3 : (Syntax.arg list))) = _menhir_stack in
            let _v : (Syntax.arg) = 
# 69 "compiler/parser/parser.mly"
                                      ( Atom (_1, _3) )
# 357 "compiler/parser/parser.ml"
             in
            _menhir_goto_normal_atom _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (_1 : (Syntax.arg))), _, (_3 : (Syntax.arg list))) = _menhir_stack in
        let _v : (Syntax.arg list) = 
# 52 "compiler/parser/parser.mly"
                         ( _1::_3 )
# 373 "compiler/parser/parser.ml"
         in
        _menhir_goto_args_inner _menhir_env _menhir_stack _menhir_s _v
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Syntax.arg list))) = _menhir_stack in
            let _v : (Syntax.arg) = 
# 79 "compiler/parser/parser.mly"
     ( List.fold_right (fun e l -> Atom (".", [e; l])) _2 (Atom ("[]", [])) )
# 389 "compiler/parser/parser.ml"
             in
            _menhir_goto_cons_atom _menhir_env _menhir_stack _menhir_s _v
        | VBAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | AtomName _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
            | LBRACKET ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | LinkName _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_atom : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.arg) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState36 | MenhirState21 | MenhirState4 | MenhirState15 | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce2 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _), _, (_3 : (Syntax.arg))) = _menhir_stack in
            let _v : (Syntax.arg) = 
# 75 "compiler/parser/parser.mly"
                                ( _3 )
# 436 "compiler/parser/parser.ml"
             in
            _menhir_goto_cons_atom _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState0 | MenhirState45 | MenhirState3 | MenhirState26 | MenhirState38 | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLMIN | COMMA | DOT | EOF | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (_1 : (Syntax.arg))) = _menhir_stack in
            let _v : (Syntax.arg) = 
# 105 "compiler/parser/parser.mly"
         ( _1 )
# 456 "compiler/parser/parser.ml"
             in
            _menhir_goto_graph _menhir_env _menhir_stack _menhir_s _v
        | EQ ->
            _menhir_reduce2 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_normal_atom : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.arg) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (Syntax.arg)) = _v in
    let _v : (Syntax.arg) = 
# 83 "compiler/parser/parser.mly"
                 ( _1 )
# 476 "compiler/parser/parser.ml"
     in
    _menhir_goto_atom _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_program : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.proc) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EOF ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (_1 : (Syntax.proc))) = _menhir_stack in
        let _v : (Syntax.proc) = 
# 41 "compiler/parser/parser.mly"
                 ( _1 )
# 494 "compiler/parser/parser.ml"
         in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_1 : (Syntax.proc)) = _v in
        Obj.magic _1
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_arg : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.arg) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState4 | MenhirState15 | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | AtomName _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
            | LBRACKET ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState15
            | LinkName _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState15)
        | RBRACKET | RPAREN | VBAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (_1 : (Syntax.arg))) = _menhir_stack in
            let _v : (Syntax.arg list) = 
# 51 "compiler/parser/parser.mly"
        ( [_1] )
# 537 "compiler/parser/parser.ml"
             in
            _menhir_goto_args_inner _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (_2 : (Syntax.arg list))), _, (_4 : (Syntax.arg))) = _menhir_stack in
            let _v : (Syntax.arg) = 
# 77 "compiler/parser/parser.mly"
     ( List.fold_right (fun e l -> Atom (".", [e; l])) _2 _4 )
# 559 "compiler/parser/parser.ml"
             in
            _menhir_goto_cons_atom _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState0 | MenhirState45 | MenhirState3 | MenhirState26 | MenhirState38 | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | AtomName _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
            | LBRACKET ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState36
            | LinkName _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (_1 : (Syntax.arg))), _, (_3 : (Syntax.arg))) = _menhir_stack in
        let _v : (Syntax.arg) = 
# 106 "compiler/parser/parser.mly"
               ( Atom ("=", [_1; _3]) )
# 601 "compiler/parser/parser.ml"
         in
        _menhir_goto_graph _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_cons_atom : _menhir_env -> 'ttv_tail -> _menhir_state -> (Syntax.arg) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (Syntax.arg)) = _v in
    let _v : (Syntax.arg) = 
# 84 "compiler/parser/parser.mly"
                 ( _1 )
# 615 "compiler/parser/parser.ml"
     in
    _menhir_goto_atom _menhir_env _menhir_stack _menhir_s _v

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "compiler/parser/parser.mly"
       (string)
# 622 "compiler/parser/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack)
    | COLMIN | COMMA | DOT | EOF | RBRACKET | RPAREN | VBAR ->
        _menhir_reduce19 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce19 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 8 "compiler/parser/parser.mly"
       (string)
# 643 "compiler/parser/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (_1 : (
# 8 "compiler/parser/parser.mly"
       (string)
# 649 "compiler/parser/parser.ml"
    ))) = _menhir_stack in
    let _v : (Syntax.arg) = 
# 67 "compiler/parser/parser.mly"
                                ( Atom (_1, []) )
# 654 "compiler/parser/parser.ml"
     in
    _menhir_goto_normal_atom _menhir_env _menhir_stack _menhir_s _v

and _menhir_run7 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 8 "compiler/parser/parser.mly"
       (string)
# 661 "compiler/parser/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AtomName _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | LBRACKET ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | LinkName _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | RPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState7 in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (_1 : (
# 8 "compiler/parser/parser.mly"
       (string)
# 681 "compiler/parser/parser.ml"
        ))) = _menhir_stack in
        let _v : (Syntax.arg) = 
# 68 "compiler/parser/parser.mly"
                                ( Atom (_1, []) )
# 686 "compiler/parser/parser.ml"
         in
        _menhir_goto_normal_atom _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState0 in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _v : (Syntax.proc) = 
# 138 "compiler/parser/parser.mly"
    ( 
      let message =
        Printf.sprintf 
          "parse error near characters %d-%d"
          (Parsing.symbol_start ())
	        (Parsing.symbol_end ())
	    in
	    failwith message
	  )
# 758 "compiler/parser/parser.ml"
         in
        _menhir_goto_program _menhir_env _menhir_stack _menhir_s _v

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 9 "compiler/parser/parser.mly"
       (string)
# 765 "compiler/parser/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 9 "compiler/parser/parser.mly"
       (string)
# 773 "compiler/parser/parser.ml"
    )) = _v in
    let _v : (Syntax.arg) = 
# 57 "compiler/parser/parser.mly"
             ( Link _1 )
# 778 "compiler/parser/parser.ml"
     in
    _menhir_goto_arg _menhir_env _menhir_stack _menhir_s _v

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AtomName _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | LBRACKET ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | LPAREN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | LinkName _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AtomName _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | LBRACKET ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | LinkName _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | RBRACKET ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState4 in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _v : (Syntax.arg) = 
# 74 "compiler/parser/parser.mly"
                      ( Atom ("[]", []) )
# 822 "compiler/parser/parser.ml"
         in
        _menhir_goto_cons_atom _menhir_env _menhir_stack _menhir_s _v
    | VBAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState4 in
        let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AtomName _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
        | LBRACKET ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState5
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState4

and _menhir_run25 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "compiler/parser/parser.mly"
       (string)
# 848 "compiler/parser/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ATAT ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AtomName _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
        | LBRACKET ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | LPAREN ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | LinkName _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26)
    | LPAREN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack)
    | COLMIN | COMMA | DOT | EOF | EQ | RPAREN ->
        _menhir_reduce19 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and main : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Syntax.proc) =
  fun lexer lexbuf ->
    let _menhir_env = {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = Obj.magic ();
      _menhir_error = false;
    } in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AtomName _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | LBRACKET ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LPAREN ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LinkName _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

# 269 "<standard.mly>"
  

# 923 "compiler/parser/parser.ml"
