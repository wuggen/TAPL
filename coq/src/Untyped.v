(* Ch3 Untyped Arithmetic Expressions *)

Set Implicit Arguments.
Set Asymmetric Patterns.

Require Import Ensembles Finite_sets Finite_sets_facts Powerset Powerset_facts.
Require Import Arith Omega.

Require Import Tapl.Sets.

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
      In _ (Union _ (consts t1) (Union _ (consts t2) (consts t3))) con ->
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
Proof.
  refine (fix consts_dec t con :=
    match t, con with
    | TTrue, TTrue | TFalse, TFalse | TO, TO => Yes
    | TSucc t', _ | TPred t', _ | TIsZero t', _ => Reduce (consts_dec t' con)
    | TIfThenElse t1 t2 t3, _ =>
        if consts_dec t1 con
          then Yes
          else if consts_dec t2 con
            then Yes
            else if consts_dec t3 con
              then Yes
              else No
    | _, _ => No
    end);
    match goal with
    | [ |- consts _ _ ] => constructor
    | [ |- ~ consts _ _ ] => inversion 1
    end; sets_auto.
Defined.

Inductive is_const: term -> Prop :=
  | TrueConst: is_const TTrue
  | FalseConst: is_const TFalse
  | OConst: is_const TO.

Hint Constructors is_const: core.

Ltac decide_consts :=
  intros; match goal with
  | [ H: consts ?t ?con |- _ ] => destruct (consts_dec t con); inversion H
  | [ H: In _ (consts ?t) ?con |- _ ] => destruct (consts_dec t con); inversion H
  | [ H: is_const _ |- _ ] => inversion H
  end; sets_auto.

Lemma consts_subset: forall t, Included _ (consts t) (Triple _ TTrue TFalse TO).
Proof.
  induction t; sets_auto.

  inversion H; sets_auto.
  inversion H; sets_auto.
  inversion H.
Qed.

Hint Resolve consts_subset: core.

Lemma card_const_le_3: forall t n, cardinal _ (consts t) n -> n <= 3.
Proof.
  intros; specialize (@consts_subset t); eapply incl_card_le; sets_auto.
  replace (Triple _ TTrue TFalse TO) with (Add _ (Add _ (Add _ (Empty_set _) TO) TFalse) TTrue).
  constructor. constructor. constructor. constructor.
  eauto with sets.
  inversion 1; inversion H1.
  inversion 1; inversion H1; inversion H3.
  
  sets_auto. inversion H0. inversion H1. inversion H3.
  inversion H5.
  inversion H5. apply Triple_r.
  inversion H3. apply Triple_m.
  inversion H1. apply Triple_l.

  inversion H0.
  apply Union_intror. apply In_singleton.
  apply Union_introl, Union_intror. apply In_singleton.
  do 2 apply Union_introl. apply Union_intror. apply In_singleton.
Qed.

Definition is_const_dec: forall t, {is_const t} + {~ is_const t}.
Proof.
  refine (fun t =>
    if term_eq_dec t TTrue
      then Yes
      else if term_eq_dec t TFalse
        then Yes
        else if term_eq_dec t TO
          then Yes
          else No); subst;
  match goal with
  | [ |- is_const _ ] => constructor
  | [ |- ~ is_const _ ] => inversion 1; congruence
  end.
Defined.

Lemma no_nonconsts_in_consts: forall t t', In _ (consts t) t' -> is_const t'.
Proof.
  induction t; sets_auto; decide_consts.
Qed.

Hint Resolve consts_subset no_nonconsts_in_consts: core.

Lemma card_consts_singleton: forall t, is_const t -> consts t = Singleton _ t.
Proof.
  inversion 1; sets_auto;
  match goal with
  | [ H: In _ (consts _) _ |- _ ] => inversion H
  | [ H: In _ (Singleton _ _) _ |- _ ] => inversion H
  end; sets_auto.
Qed.

Hint Resolve card_consts_singleton: core.

Definition card_consts_dec: forall t, {n: nat | cardinal _ (consts t) n}.
Proof.



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
