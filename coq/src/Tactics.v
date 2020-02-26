From Ltac2 Require Import Ltac2 Notations.
From Ltac2 Require Message String Char List Std Control.

Ltac2 msg_string s := Message.of_string s.
Ltac2 msg_int n := Message.of_int n.
Ltac2 msg_ident id := Message.of_ident id.
Ltac2 msg_constr c := Message.of_constr c.
Ltac2 msg_exn e := Message.of_exn e.

Ltac2 Notation a(self) ":+:" b(self) := Message.concat a b.

Ltac2 msg_char c := msg_string (String.make 1 c).
Ltac2 newline0 () := msg_char (Char.of_int 10).
Ltac2 Notation newline := newline0 ().

Ltac2 print := Message.print.
Ltac2 println m := Message.print (m :+: newline).

Ltac2 print_string s := print (msg_string s).
Ltac2 println_string s := println (msg_string s).
Ltac2 print_int n := print (msg_int n).
Ltac2 println_int n := println (msg_int n).
Ltac2 print_ident id := print (msg_ident id).
Ltac2 println_ident id := println (msg_ident id).
Ltac2 print_constr c := print (msg_constr c).
Ltac2 println_constr c := println (msg_constr c).
Ltac2 print_exn e := print (msg_exn e).
Ltac2 println_exn e := println (msg_exn e).

Ltac2 msg_fold ms := List.fold_left (Message.concat) ms (msg_string "").

Ltac2 msg_join (m: message) (ms: message list) :=
    let rec msg_join' ms := match ms with
    | [] => []
    | h :: t => match t with
        | [] => h :: []
        | _ :: _ => h :: m :: msg_join' t
        end
    end in
    let ms' := msg_join' ms
    in msg_fold ms'.

Ltac2 msg_lines ms := msg_join newline ms.

Ltac2 alternation (a: unit -> unit) (b: unit -> unit) :=
    Control.plus a (fun _ => b ()).

Ltac2 Notation a(self) "||" b(self) := alternation (fun _ => a) (fun _ => b).

Ltac2 Notation "simple" "rawinversion"
  arg(self)
  pat(opt(seq("as", intropattern)))
  ids(opt(seq("in", list1(ident)))) :=
  Std.inversion Std.SimpleInversion (Std.ElimOnIdent arg) pat ids.

Ltac2 Notation "rawinversion"
  arg(self)
  pat(opt(seq("as", intropattern)))
  ids(opt(seq("in", list1(ident)))) :=
  Std.inversion Std.FullInversion (Std.ElimOnIdent arg) pat ids.

Ltac2 Notation "rawinversion_clear"
  arg(self)
  pat(opt(seq("as", intropattern)))
  ids(opt(seq("in", list1(ident)))) :=
  Std.inversion Std.FullInversionClear (Std.ElimOnIdent arg) pat ids.

Ltac2 sig_auto0 () :=
    repeat (match! goal with
        | [ h: { _ : _ | _ } |- _ ] => destruct h
        end); auto.
Ltac2 Notation sig_auto := sig_auto0 ().

Ltac2 conj_disj_elim tac :=
    repeat (match! goal with
        | [ h: _ /\ _ |- _ ] => destruct h
        | [ h: _ \/ _ |- _ ] => destruct h
        | [ |- _ /\ _ ] => split
        | [ |- _ \/ _ ] => (left; progress tac) || (right; progress tac) || ()
        end); auto.
