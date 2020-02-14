(* Ch3 Untyped Arithmetic Expressions *)

Set Implicit Arguments.
Set Asymmetric Patterns.

Require Import Ensembles Finite_sets.
Require Import Arith.

Inductive term: Set :=
  | TTrue: term
  | TFalse: term
  | TIfThenElse: term -> term -> term -> term
  | TO: term
  | TSucc: term -> term
  | TPred: term -> term
  | TIsZero: term -> term.

Inductive consts: term -> Ensemble term :=
  | CTrue: consts TTrue TTrue
  | CFalse: consts TFalse TFalse
  | CO: consts TO TO
  | CSucc: forall t con, consts t con -> consts (TSucc t) con
  | CPred: forall t con, consts t con -> consts (TPred t) con
  | CIsZero: forall t con, consts t con -> consts (TIsZero t) con
  | CIfThenElse: forall t1 t2 t3 con,
      consts t1 con \/ consts t2 con \/ consts t3 con ->
      consts (TIfThenElse t1 t2 t3) con.

Inductive size: term -> nat -> Prop :=
  | STrue: size TTrue 1
  | SFalse: size TFalse 1
  | SO: size TO 1
  | SSucc: forall t n, size t n -> size (TSucc t) (S n)
  | SPred: forall t n, size t n -> size (TPred t) (S n)
  | SIsZero: forall t n, size t n -> size (TIsZero t) (S n)
  | SIfThenElse: forall t1 t2 t3 n1 n2 n3,
      size t1 n1 -> size t2 n2 -> size t3 n3 ->
      size (TIfThenElse t1 t2 t3) (n1 + n2 + n3 + 1).

Inductive depth: term -> nat -> Prop :=
  | DTrue: depth TTrue 1
  | DFalse: depth TFalse 1
  | DO: depth TO 1
  | DSucc: forall t n, depth t n -> depth (TSucc t) (S n)
  | DPred: forall t n, depth t n -> depth (TPred t) (S n)
  | DIsZero: forall t n, depth t n -> depth (TIsZero t) (S n)
  | DIfThenElse: forall t1 t2 t3 n1 n2 n3,
      depth t1 n1 -> depth t2 n2 -> depth t3 n3 ->
      depth (TIfThenElse t1 t2 t3) (S (max n1 (max n2 n3))).

Hint Constructors term consts size depth: core.
Hint Constructors cardinal: sets.

Notation "'Yes'" := (left _).
Notation "'No'" := (right _).
Notation "'Reduce' x" := (if x then Yes else No) (at level 50).
Notation "[ x ]" := (exist _ x _).

Definition term_eq_dec: forall t1 t2: term, {t1 = t2} + {t1 <> t2}.
Proof.
  refine (fix term_eq t1 t2 :=
    match t1, t2 with
    | TTrue, TTrue => Yes
    | TFalse, TFalse => Yes
    | TO, TO => Yes
    | TSucc t1', TSucc t2' => Reduce (term_eq t1' t2')
    | TPred t1', TPred t2' => Reduce (term_eq t1' t2')
    | TIsZero t1', TIsZero t2' => Reduce (term_eq t1' t2')
    | TIfThenElse t1a t1b t1c, TIfThenElse t2a t2b t2c =>
        if term_eq t1a t2a
          then if term_eq t1b t2b
            then if term_eq t1c t2c
              then Yes
              else No
            else No
          else No
    | _, _ => No
    end); congruence.
Qed.

Definition consts_dec: forall t con, {consts t con} + {~ consts t con}.
Abort.

Ltac sig_auto :=
  repeat match goal with
  | [ H: { _ : _ | _ } |- _ ] => destruct H
  end; auto.

Definition size_dec: forall t, {n: nat | size t n}.
Proof.
  refine (fix size_dec t :=
      match t with
      | TTrue | TFalse | TO => [ 1 ]
      | TSucc t' | TPred t' | TIsZero t' =>
          let s := size_dec t' in
          [ S (proj1_sig s) ]
      | TIfThenElse t1 t2 t3 =>
          let s1 := size_dec t1 in
          let s2 := size_dec t2 in
          let s3 := size_dec t3 in
          [ (proj1_sig s1) + (proj1_sig s2) + (proj1_sig s3) + 1 ]
      end); sig_auto.
Defined.

Definition depth_dec: forall t, {n: nat | depth t n}.
Proof.
  refine (fix depth_dec t :=
    match t with
    | TTrue | TFalse | TO => [ 1 ]
    | TSucc t' | TPred t' | TIsZero t' =>
        let d := depth_dec t' in
        [ S (proj1_sig d) ]
    | TIfThenElse t1 t2 t3 =>
        let d1 := depth_dec t1 in
        let d2 := depth_dec t2 in
        let d3 := depth_dec t3 in
        [ S (max (proj1_sig d1) (max (proj1_sig d2) (proj1_sig d3))) ]
    end); sig_auto.
Qed.

Lemma consts_le_size: forall t c s,
  cardinal _ (consts t) c -> size t s -> c <= s.
Proof.
  induction t; auto with sets.
