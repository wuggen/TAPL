Notation "'Yes'" := (left _).
Notation "'No'" := (right _).
Notation "'Reduce' x" := (if x then Yes else No) (at level 50).
Notation "[ x ]" := (exist _ x _).
