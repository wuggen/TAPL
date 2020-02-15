Set Implicit Arguments.
Set Asymmetric Patterns.

Require Export Ensembles Finite_sets Finite_sets_facts.

Ltac sig_auto :=
  repeat match goal with
  | [ H: { _ : _ | _ } |- _ ] => destruct H
  end; auto.

Ltac conj_disj_elim tac :=
  repeat match goal with
    | [ H: _ \/ _ |- _ ] => destruct H
    | [ H: _ /\ _ |- _ ] => destruct H
    | [ |- _ /\ _ ] => split
    | [ |- _ \/ _ ] => (left; progress tac) || (right; progress tac) || idtac
    end.

Ltac sets_basic :=
  intros;
  match goal with
  | [ |- _ = _ ] => apply Extensionality_Ensembles
  | [ |- ~ _ ] => intro
  | [ |- context[In _ (Singleton _ _) _] ] => apply In_singleton
  | [ |- context[Couple _ _ _] ] => rewrite <- Couple_as_union
  | [ |- context[Triple _ _ _ _] ] => rewrite <- Triple_as_union
  | [ H: In _ (Singleton _ _) _ |- _ ] => inversion H
  | [ H: In _ (Couple _ _ _) _ |- _ ] => inversion H
  | [ H: In _ (Triple _ _ _ _) _ |- _ ] => inversion H
  | [ H: In _ (Union _ _ _) _ |- _ ] => inversion H
  end;
  repeat (unfold Same_set in * || unfold Included in * );
  eauto with sets;
  firstorder.

Ltac search_union tac :=
  let rec search := tac ||
    match goal with
    | [ |- In _ (Union _ _ _) _ ] =>
        progress tac
        || (apply Union_introl; search)
        || (apply Union_intror; search)
    end in
  repeat search.

Ltac search_couple tac :=
  let rec search := tac ||
    match goal with
    | [ |- In _ (Couple _ _ _) _ ] =>
        progress tac
        || (apply Couple_l; search)
        || (apply Couple_r; search)
    end in
  repeat search.

Ltac search_triple tac :=
  let rec search := tac ||
    match goal with
    | [ |- In (Triple _ _ _ _) _ ] =>
        progress tac
        || (apply Triple_l; search)
        || (apply Triple_m; search)
        || (apply Triple_r; search)
    end in
  repeat search.

Section sets.
  Variable U: Type.
  Variable x y z: U.

  Ltac solve :=
    sets_basic;
    rewrite Empty_set_zero' in *;
    unfold Add in *;
    repeat match goal with
    | [ H: In _ (Singleton _ _) _ |- _ ] => inversion H; clear H
    | [ H: In _ (Couple _ _ _) _ |- _ ] => inversion H; clear H
    | [ H: In _ (Triple _ _ _ _) _ |- _ ] => inversion H; clear H
    | [ H: In _ (Union _ _ _) _ |- _ ] => inversion H; clear H
    end;
    match goal with
    | [ |- context[Singleton] ] => apply In_singleton
    | [ |- context[Couple] ] => search_couple ltac:(solve)
    | [ |- context[Triple] ] => search_triple ltac:(solve)
    | [ |- context[Union] ] => search_union ltac:(solve)
    end.

  Lemma Singleton_Add: Singleton _ x = Add _ (Empty_set _) x.
  Proof. solve. Qed.

  Lemma Couple_Add: Couple _ x y = Add _ (Add _ (Empty_set _) x) y.
  Proof. solve. Qed.

  Lemma Triple_Add: Triple _ x y z = Add _ (Add _ (Add _ (Empty_set _) x) y) z.
  Proof. solve. Qed.
End sets.

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

Notation "'Yes'" := (left _).
Notation "'No'" := (right _).
Notation "'Reduce' x" := (if x then Yes else No) (at level 50).
Notation "[ x ]" := (exist _ x _).
