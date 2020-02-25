Set Implicit Arguments.
Set Asymmetric Patterns.

Require Export Ensembles Finite_sets Finite_sets_facts.

Require Import Tapl.Tactics.
From Ltac2 Require Import Ltac2 Notations.
From Ltac2 Require Control.

Ltac2 rec union_search (tac: unit -> unit) :=
    let app t := apply (ltac2:(t)); orelse (fun _ => progress (tac ())) (fun _ => union_search tac) in
    app (constr:(Union_introl)) || app (constr:(Union_intror)).

Ltac2 rec sets_basic0 () :=
    Control.enter (fun _ => intros; repeat (match! goal with
    | [ |- _ = _ ] =>
        apply Extensionality_Ensembles;
        unfold Same_set; unfold Included;
        split; intros
    | [ |- ~ _ ] => unfold not; intro
    | [ |- In _ (Singleton _ _) _ ] => apply In_singleton
    | [ |- context[Couple _ _ _] ] => rewrite <- Couple_as_union
    | [ |- context[Triple _ _ _ _] ] => rewrite <- Triple_as_union
    | [ h: In _ (Singleton _ _) _ |- _ ] => rawinversion_clear h
    | [ h: In _ (Couple _ _ _) _ |- _ ] => rawinversion_clear h
    | [ h: In _ (Triple _ _ _ _) _ |- _ ] => rawinversion_clear h
    | [ |- context[Add _ (Empty_set _) _] ] => rewrite Empty_set_zero'
    | [ h: context[Add _ (Empty_set _) _] |- _] => rewrite Empty_set_zero' in *
    end);
    auto with sets).
Ltac2 Notation sets_basic := sets_basic0 ().

Section sets.
  Variable U: Type.
  Variable x y z: U.

  Lemma Singleton_Add: Singleton _ x = Add _ (Empty_set _) x.
  Proof. sets_basic. Qed.

  Lemma Couple_Add: Couple _ x y = Add _ (Add _ (Empty_set _) x) y.
  Proof. sets_basic. Qed.

  Lemma Triple_Add: Triple _ x y z = Add _ (Add _ (Add _ (Empty_set _) x) y) z.
  Proof. sets_basic. Qed.
End sets.

(*
Ltac sets_rewrite tac :=
  match goal with
  | [ |- context[Union _ (Union _ _ _) _] ] => rewrite Union_associative; progress tac
  | [ |- context[Singleton _ _] ] => rewrite Singleton_Add
  | [ |- context[Couple _ _ _] ] => rewrite Couple_Add
  | [ |- context[Triple _ _ _ _] ] => rewrite Triple_Add
  | [ |- context[Add _ (Empty_set _) _] ] => rewrite Empty_set_zero'; progress tac
  end.

Ltac sets_auto :=
  try sets_basic;
  try sets_rewrite sets_basic;
  repeat (
    sets_basic;
    conj_disj_elim ltac:(sets_basic);
    search_union sets_basic;
    search_couple sets_basic;
    search_triple sets_basic;
    eauto with sets);
  try sets_rewrite sets_basic;
  eauto with sets.
*)

Notation "'Yes'" := (left _).
Notation "'No'" := (right _).
Notation "'Reduce' x" := (if x then Yes else No) (at level 50).
Notation "[ x ]" := (exist _ x _).
