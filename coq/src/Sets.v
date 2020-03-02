Set Implicit Arguments.
Set Asymmetric Patterns.

Require Export Ensembles Finite_sets Finite_sets_facts.

Require Import Tapl.Tactics.

Ltac union_search tac :=
    (apply Union_introl; progress tac) || (apply Union_intror; progress tac).

Ltac inhabited_auto :=
    repeat match goal with
    | [ h: Inhabited _ _ |- _ ] => inversion_clear h
    | [ h: In _ ?S ?x |- Inhabited _ (Union _ ?S _) ] => apply (Inhabited_intro _ _ x), Union_introl
    | [ h: In _ ?S ?x |- Inhabited _ (Union _ _ ?S) ] => apply (Inhabited_intro _ _ x), Union_intror
    end; eauto with sets.

Ltac sets_basic :=
    intros; eauto with sets;
    repeat match goal with
    | [ |- _ = _ ] => apply Extensionality_Ensembles
    | [ |- Same_set _ _ _ ] => unfold Same_set; unfold Included; split; intros
    | [ |- Included _ _ _ ] => unfold Included; intros
    | [ h: Same_set _ _ _ |- _ ] => unfold Same_set in h; unfold Included in h; destruct h
    | [ h: Included _ _ _ |- _ ] => unfold Included in h
    | [ |- ~ _ ] => unfold not; intro
    | [ |- In _ (Singleton _ _) _ ] => apply In_singleton
    | [ |- context[Couple _ _ _] ] => rewrite <- Couple_as_union
    | [ |- context[Triple _ _ _ _] ] => rewrite <- Triple_as_union
    | [ h: context[Couple _ _ _] |- _ ] => rewrite <- Couple_as_union in h
    | [ h: context[Triple _ _ _ _] |- _ ] => rewrite <- Triple_as_union in h
    | [ h: In _ (Empty_set _) _ |- _ ] => inversion h; clear h
    | [ h: In _ (Singleton _ _) _ |- _ ] => inversion h; clear h
    | [ h: In _ (Couple _ _ _) _ |- _ ] => inversion h; clear h
    | [ h: In _ (Triple _ _ _ _) _ |- _ ] => inversion h; clear h
    | [ h: In _ (Union _ _ _) _ |- _ ] => inversion h; clear h
    | [ h: In _ (Add _ _ _) _ |- _ ] => inversion h; clear h
    end; eauto with sets;
    inhabited_auto.

Ltac sets_auto1 :=
    repeat (sets_basic; conj_disj_elim sets_basic);
    repeat match goal with
    | [ |- In _ (Union _ _ _) _ ] => union_search sets_basic
    | [ |- In _ (Add _ _ _) _ ] => union_search sets_basic
    end;
    eauto with sets.

Section sets.
  Variable U: Type.
  Variable x y z: U.
  Variables A B: Ensemble U.

  Lemma Singleton_Add: Singleton _ x = Add _ (Empty_set _) x.
  Proof. sets_auto1. Qed.

  Lemma Couple_Add: Couple _ x y = Add _ (Add _ (Empty_set _) x) y.
  Proof. sets_auto1. Qed.

  Lemma Triple_Add: Triple _ x y z = Add _ (Add _ (Add _ (Empty_set _) x) y) z.
  Proof. sets_auto1. Qed.

  Lemma Union_or: In _ A x \/ In _ B x -> In _ (Union _ A B) x.
  Proof. sets_auto1. Qed.

  Lemma Inhabited_Union: Inhabited _ A \/ Inhabited _ B -> Inhabited _ (Union _ A B).
  Proof. sets_auto1. Qed.

  Lemma Union_Singleton_Add_l: Union _ (Singleton _ x) A = Add _ A x.
  Proof. sets_auto1. Qed.

  Lemma Union_Singleton_Add_r: Union _ A (Singleton _ x) = Add _ A x.
  Proof. sets_auto1. Qed.
End sets.

Hint Resolve Singleton_Add Couple_Add Triple_Add Union_or Inhabited_Union: sets.

Ltac cardinal_auto :=
    repeat match goal with
    | [ |- cardinal _ (Empty_set _) 0 ] => constructor
    | [ |- cardinal _ (Add _ _ _) _ ] => constructor

    | [ |- cardinal _ (Singleton _ _) _ ] => rewrite Singleton_Add
    | [ |- cardinal _ (Couple _ _ _) _ ] => rewrite Couple_Add
    | [ |- cardinal _ (Triple _ _ _ _) _ ] => rewrite Triple_Add

    | [ |- cardinal _ (Union _ (Singleton _ _) _) _ ] => rewrite Union_Singleton_Add_l
    | [ |- cardinal _ (Union _ _ (Singleton _ _)) _ ] => rewrite Union_Singleton_Add_r
    end; eauto with sets.

Ltac sets_auto :=
    intros;
    cardinal_auto;
    sets_auto1.

Section sets.
  Variable U: Type.
  Variables x y z: U.
  Variables A B: Ensemble U.

  Lemma card_Singleton: cardinal _ (Singleton _ x) 1.
  Proof. sets_auto. Qed.

  Lemma card_Couple: x <> y -> cardinal _ (Couple _ x y) 2.
  Proof. sets_auto. Qed.

  Lemma card_Triple: x <> y -> y <> z -> x <> z -> cardinal _ (Triple _ x y z) 3.
  Proof. sets_auto. Qed.
End sets.

Hint Resolve card_Singleton card_Couple card_Triple: sets.
