(* Ch3 Untyped Arithmetic Expressions *)

Set Implicit Arguments.
Set Asymmetric Patterns.

Require Import Ensembles Finite_sets Finite_sets_facts Powerset Powerset_facts.
Require Import Arith Omega.

From Tapl Require Import Sets Tactics Notations.

Inductive term: Set :=
  | TTrue: term
  | TFalse: term
  | TIfThenElse: term -> term -> term -> term
  | TO: term
  | TSucc: term -> term
  | TPred: term -> term
  | TIsZero: term -> term.

Inductive bool_val: term -> Prop :=
  | BVTrue: bool_val TTrue
  | BVFalse: bool_val TFalse.

Inductive nat_val: term -> Prop :=
  | NVO: nat_val TO
  | NVSucc: forall t, nat_val t -> nat_val (TSucc t).

Inductive value: term -> Prop :=
  | VBool: forall t, bool_val t -> value t
  | VNat: forall t, nat_val t -> value t.

Hint Constructors bool_val nat_val value: core.

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
    end); simpl;
    match goal with
    | [ |- _ = _ ] => try reflexivity
    | [ |- _ <> _ ] => unfold not; try (inversion 1)
    end;
    subst; try discriminate; auto with sets.
Qed.

Definition consts_dec: forall t con, {In _ (consts t) con} + {~ In _ (consts t) con}.
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
    | [ |- In _ (consts _) _ ] => constructor
    | [ |- ~ In _ (consts _) _ ] => inversion 1
    end; sets_auto.
Defined.

Inductive is_const: term -> Prop :=
  | TrueConst: is_const TTrue
  | FalseConst: is_const TFalse
  | OConst: is_const TO.

Hint Constructors is_const: core.

Ltac consts_auto :=
  subst; intros; try match goal with
  | [ h: consts ?t ?con |- _ ] => destruct (consts_dec t con); inversion h
  | [ h: In _ (consts ?t) ?con |- _ ] => destruct (consts_dec t con); inversion h
  | [ h: is_const _ |- _ ] => inversion h
  end;
  repeat match goal with
  | [ h: context[consts_dec ?t ?con] |- _ ] => destruct (consts_dec t con)
  | [ x := context[consts_dec ?t ?con] |- _ ] => destruct (consts_dec t con)
  end; sets_auto.

Lemma consts_TSucc: forall t, consts (TSucc t) = consts t.
Proof. sets_auto; consts_auto. Qed.

Lemma consts_TPred: forall t, consts (TPred t) = consts t.
Proof. sets_auto; consts_auto. Qed.

Lemma consts_TIsZero: forall t, consts (TIsZero t) = consts t.
Proof. sets_auto; consts_auto. Qed.

Lemma consts_TIfThenElse: forall t1 t2 t3,
  consts (TIfThenElse t1 t2 t3) = Union _ (consts t1) (Union _ (consts t2) (consts t3)).
Proof. sets_auto; consts_auto. Qed.

Lemma consts_singleton: forall t, is_const t -> consts t = Singleton _ t.
Proof.
  inversion 1; sets_auto; consts_auto.
Qed.

Lemma consts_subset: forall t, Included _ (consts t) (Triple _ TTrue TFalse TO).
Proof.
  induction t; sets_auto; consts_auto.
Qed.

Hint Resolve consts_subset: core.
Hint Extern 1 =>
  match goal with
  | [ |- context[consts (TSucc _)] ] => rewrite consts_TSucc
  | [ |- context[consts (TPred _)] ] => rewrite consts_TPred
  | [ |- context[consts (TIsZero _)] ] => rewrite consts_TIsZero
  | [ |- context[consts (TIfThenElse _ _ _)] ] => rewrite consts_TIfThenElse
  | [ |- context[consts TTrue] ] => rewrite consts_singleton
  | [ |- context[consts TFalse] ] => rewrite consts_singleton
  | [ |- context[consts TO] ] => rewrite consts_singleton
  end: core.

Lemma Inhabited_consts: forall t, Inhabited _ (consts t).
Proof.
  induction t;
  try match goal with
  | [ |- context[consts TTrue] ] => apply (Inhabited_intro _ _ TTrue)
  | [ |- context[consts TFalse] ] => apply (Inhabited_intro _ _ TFalse)
  | [ |- context[consts TO] ] => apply (Inhabited_intro _ _ TO)
  end; sets_auto.
Qed.

Lemma card_const_le_3: forall t n, cardinal _ (consts t) n -> n <= 3.
Proof.
  intros; specialize (@consts_subset t); apply incl_card_le; sets_auto.
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
  induction t; sets_auto; consts_auto.
Qed.

Hint Resolve consts_subset no_nonconsts_in_consts: core.

Definition card_consts_dec: forall t, {n: nat | cardinal _ (consts t) n}.
Proof.
  refine (fun t =>
      let n1 := if consts_dec t TTrue
        then 1 else 0
      in let n2 := if consts_dec t TFalse
        then S n1 else n1
      in let n3 := if consts_dec t TO
        then S n2 else n2
      in [n3]).
Abort.
 
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

(** * Evaluation *)

Inductive eval_bool: term -> term -> Prop :=
  | EBIfTrue: forall t1 t2, eval_bool (TIfThenElse TTrue t1 t2) t1
  | EBIfFalse: forall t1 t2, eval_bool (TIfThenElse TFalse t1 t2) t2
  | EBIf: forall cond cond', eval_bool cond cond' ->
      forall t1 t2, eval_bool (TIfThenElse cond t1 t2) (TIfThenElse cond' t1 t2).

Notation "x +-> y" := (eval_bool x y) (at level 90).
Hint Constructors eval_bool: core.

Theorem eval_bool_det: forall t t': term, (t +-> t') ->
  forall t'', (t +-> t'') -> t' = t''.
Proof.
  induction 1; intros;
  match goal with
  | [ H: _ +-> _ |- _ ] => inversion H; try reflexivity; subst
  end;
  match goal with
  | [ H: TTrue +-> _ |- _ ] => inversion H
  | [ H: TFalse +-> _ |- _ ] => inversion H
  | [ IH: forall t, ?c +-> t -> ?c' = t,
      H: ?c +-> ?t' |- _ ] => rewrite (IH t' H)
  end;
  auto.
Qed.

Hint Resolve eval_bool_det: core.

Definition bool_normal_form (t: term) : Prop := forall t', ~ (t +-> t').

Hint Unfold bool_normal_form: core.

Theorem bool_vals_normal_form: forall t, bool_val t -> bool_normal_form t.
Proof.
  inversion 1; autounfold with core; intros;
  match goal with
  | [ H: _ +-> _ |- _ ] => inversion H
  end.
Qed.

Inductive eval_bool_multi: term -> term -> Prop :=
  | EBMEval: forall t1 t2, (t1 +-> t2) -> eval_bool_multi t1 t2
  | EBMRefl: forall t, eval_bool_multi t t
  | EBMTrans: forall t1 t2 t3,
      eval_bool_multi t1 t2 -> eval_bool_multi t2 t3 -> eval_bool_multi t1 t3.

Notation "x +->* y" := (eval_bool_multi x y) (at level 90).
Hint Constructors eval_bool_multi: core.

Lemma eval_bool_multi_nf: forall t1 t2, bool_normal_form t1 -> (t1 +->* t2) -> t1 = t2.
Proof.
  autounfold with core; induction 2.
  - exfalso; apply (H t2), H0.
  - reflexivity.
  - rewrite IHeval_bool_multi1 in *; try apply IHeval_bool_multi2; assumption.
Qed.

Hint Resolve eval_bool_multi_nf: core.

Theorem bool_normal_form_unique: forall t u1,
  bool_normal_form u1 -> (t +->* u1) -> forall u2,
  bool_normal_form u2 -> (t +->* u2) ->
  u1 = u2.
Proof.
  autounfold with core. induction 2; intros.

  inversion H2; subst.
  - apply (eval_bool_det H0 H3).
  - exfalso; apply (H1 t2), H0.
  - inversion H3; subst.
    + rewrite (eval_bool_det H0 H5) in *. apply (eval_bool_multi_nf H H4).
    + 
