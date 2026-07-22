import RayTracing.Basic

namespace interval

open infinity

public structure Interval α where
  min : α
  max: α

namespace Interval

public def size
  [Sub α]
  (self : Interval α)
  : α :=
  self.max - self.min

public def contains
  [LE α] [DecidableLE α]
  (self : Interval α) (x : α)
  : Bool :=
  self.min ≤ x ∧ x ≤ self.max

public def surrounds
  [LT α] [DecidableLT α]
  (self : Interval α) (x : α)
  : Bool :=
  self.min < x ∧ x < self.max

public def empty
  [Neg α] [Infinity α]
  : Interval α := {
    min := infinity
    max := -infinity
  }

public def full
  [Neg α] [Infinity α]
  : Interval α := {
    min := -infinity
    max := infinity
  }

public instance
  [Neg α] [Infinity α]
  : Inhabited (Interval α) := {
    default := empty
  }

end Interval

end interval
