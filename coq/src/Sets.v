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
    | [ |- _ = _ ] => apply Extensionality_Ensembles
    | [ |- context[Same_set _ _ _] ] => unfold Same_set; unfold Included; split; intros
    | [ |- context[Included _ _ _] ] => unfold Included; intros
    | [ |- ~ _ ] => unfold not; intro
    | [ |- In _ (Singleton _ _) _ ] => apply In_singleton
    | [ |- context[Couple _ _ _] ] => rewrite <- Couple_as_union
    | [ |- context[Triple _ _ _ _] ] => rewrite <- Triple_as_union
    | [ h: In _ (Singleton _ _) _ |- _ ] => rawinversion_clear h
    | [ h: In _ (Couple _ _ _) _ |- _ ] => rawinversion_clear h
    | [ h: In _ (Triple _ _ _ _) _ |- _ ] => rawinversion_clear h
    | [ h: In _ (Union _ _ _) _ |- _ ] => rawinversion_clear h
    | [ h: In _ (Add _ _ _) _ |- _ ] => rawinversion_clear h
    | [ |- context[Add _ (Empty_set _) _] ] => rewrite Empty_set_zero'
    | [ h: context[Add _ (Empty_set _) _] |- _] => rewrite Empty_set_zero' in *
    end);
    auto with sets).
Ltac2 Notation sets_basic := sets_basic0 ().

Ltac2 sets_auto0 () :=
    sets_basic;
    repeat (match! goal with
    | [ |- In _ (Union _ _ _) _ ] => union_search sets_basic0
    | [ |- In _ (Add _ _ _) _ ] => union_search sets_basic0
    end);
    auto with sets.
Ltac2 Notation sets_auto := sets_auto0 ().

Set Ltac2 Backtrace.

Section sets.
  Variable U: Type.
  Variable x y z: U.

  Lemma Singleton_Add: Singleton _ x = Add _ (Empty_set _) x.
  Proof. sets_auto. Qed.

  Lemma Couple_Add: Couple _ x y = Add _ (Add _ (Empty_set _) x) y.
  Proof. sets_auto. Qed.

  Lemma Triple_Add: Triple _ x y z = Add _ (Add _ (Add _ (Empty_set _) x) y) z.
  Proof. sets_auto. Qed.
End sets.
