module Data.Semilattice where

-- | A join semilattice is an idempotent commutative semigroup.
class JoinSemilattice s where
  (\/) :: s -> s -> s

class MeetSemilattice s where
  (/\) :: s -> s -> s

class LowerBounded s where
  bottom :: s

class UpperBounded s where
  top :: s


instance JoinSemilattice () where
  _ \/ _ = ()

instance MeetSemilattice () where
  _ /\ _ = ()

instance LowerBounded () where
  bottom = ()

instance UpperBounded () where
  top = ()
