import RayTracing.Basic

open Sqrt

public def vec3 (x y z : α) : Vector α 3 :=
  ⟨#[x, y, z], rfl⟩

public def Vector.x (self : Vector α 3) :=
  self.get ⟨0, by decide⟩

public def Vector.y (self : Vector α 3) :=
  self.get ⟨1, by decide⟩

public def Vector.z (self : Vector α 3) :=
  self.get ⟨2, by decide⟩

public def Vector.scalarMultiply
  [HMul α β γ]
  (self : Vector α n) (scalar : β)
  : Vector γ n :=
  self.map (· * scalar)

public def Vector.scalarDivide
  [HDiv α β γ]
  (self : Vector α n) (scalar : β)
  : Vector γ n :=
  self.map (· / scalar)

public def Vector.elementWiseAdd
  [HAdd α β γ]
  (lhs : Vector α n) (rhs : Vector β n)
  : Vector γ n :=
  lhs.zipWith (· + ·) rhs

public def Vector.elementWiseSubtract
  [HSub α β γ]
  (lhs : Vector α n) (rhs : Vector β n)
  : Vector γ n :=
  lhs.zipWith (· - ·) rhs

public def Vector.elementWiseMultiply
  [HMul α β γ]
  (lhs : Vector α n) (rhs : Vector β n)
  : Vector γ n :=
  lhs.zipWith (· * ·) rhs

public def Vector.dotProduct
  [HMul α β γ] [Add γ] [Zero γ]
  (lhs : Vector α n) (rhs : Vector β n)
  : γ :=
  (lhs.elementWiseMultiply rhs).sum

public def Vector.normSquared
  [Mul α] [Add α] [Zero α]
  (self : Vector α n)
  : α :=
  self.dotProduct self

public def Vector.norm
  [Mul α] [Zero α] [Add α] [Sqrt α]
  (self : Vector α n)
  : α :=
  sqrt self.normSquared

public def Vector.normalize
  [Mul α] [Div α] [Zero α] [Add α] [Sqrt α] [BEq α]
  (self : Vector α n)
  : Option (Vector α n) := do
  let normSquared := self.normSquared
  let norm ← if normSquared == 0 then none else some (sqrt normSquared)
  self.scalarDivide norm

public def Vector.crossProduct
  [HMul α β γ] [HSub γ γ δ]
  (lhs : Vector α 3) (rhs : Vector β 3)
  : Vector δ 3 :=
  let x := lhs.y * rhs.z - lhs.z * rhs.y
  let y := lhs.z * rhs.x - lhs.x * rhs.z
  let z := lhs.x * rhs.y - lhs.y * rhs.x
  vec3 x y z

public instance [HAdd α β γ] : HAdd (Vector α n) (Vector β n) (Vector γ n) where
  hAdd := Vector.elementWiseAdd

public instance [HAdd α β γ] : HAdd (Vector β n) (Vector α n) (Vector γ n) where
  hAdd lhs rhs := rhs.elementWiseAdd lhs

public instance [HMul α β γ] : HMul (Vector α n) (β) (Vector γ n) where
  hMul := Vector.scalarMultiply

public instance [HMul α β γ] : HMul (β) (Vector α n) (Vector γ n) where
  hMul c v := v.scalarMultiply c

public instance [HDiv α β γ] : HDiv (Vector α n) (β) (Vector γ n) where
  hDiv v c := v.scalarDivide c
