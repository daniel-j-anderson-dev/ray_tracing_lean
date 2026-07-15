import RayTracing.Basic

namespace vector3

open sqrt

public structure Vector3 α where
  x : α
  y : α
  z : α

namespace Vector3

public def map
  (f: α → β) (self : Vector3 α)
  : Vector3 β :=
  ⟨f self.x, f self.y, f self.z⟩

public def zipWith
  (f : α → β → γ) (self : Vector3 α) (other : Vector3 β)
  : Vector3 γ :=
  ⟨f self.x other.x, f self.y other.y, f self.z other.z⟩

public def sumComponents
  [Add α]
  (self : Vector3 α)
  : α :=
  self.x + self.y + self.z

public def scalarMultiply
  [HMul α β γ]
  (self : Vector3 α) (scalar : β)
  : Vector3 γ :=
  self.map (· * scalar)

public def scalarDivide
  [HDiv α β γ]
  (self : Vector3 α) (scalar : β)
  : Vector3 γ :=
  self.map (· / scalar)

public def componentWiseAdd
  [HAdd α β γ]
  (lhs : Vector3 α) (rhs : Vector3 β)
  : Vector3 γ :=
  lhs.zipWith (· + ·) rhs

public def componentWiseSubtract
  [HSub α β γ]
  (lhs : Vector3 α) (rhs : Vector3 β)
  : Vector3 γ :=
  lhs.zipWith (· - ·) rhs

public def componentWiseMultiply
  [HMul α β γ]
  (lhs : Vector3 α) (rhs : Vector3 β)
  : Vector3 γ :=
  lhs.zipWith (· * ·) rhs

public def dotProduct
  [HMul α β γ] [Add γ] [Zero γ]
  (lhs : Vector3 α) (rhs : Vector3 β)
  : γ :=
  (lhs.componentWiseMultiply rhs).sumComponents

public def normSquared
  [Mul α] [Add α] [Zero α]
  (self : Vector3 α)
  : α :=
  self.dotProduct self

public def norm
  [Mul α] [Zero α] [Add α] [Sqrt α]
  (self : Vector3 α)
  : α :=
  sqrt self.normSquared

public def normalize
  [Mul α] [Div α] [Zero α] [Add α] [Sqrt α] [BEq α]
  (self : Vector3 α)
  : Option (Vector3 α) := do
  let normSquared := self.normSquared
  let norm ← if normSquared == 0 then none else some (sqrt normSquared)
  self.scalarDivide norm

public def crossProduct
  [HMul α β γ] [HSub γ γ δ]
  (lhs : Vector3 α) (rhs : Vector3 β)
  : Vector3 δ :=
  ⟨
    lhs.y * rhs.z - lhs.z * rhs.y,
    lhs.z * rhs.x - lhs.x * rhs.z,
    lhs.x * rhs.y - lhs.y * rhs.x,
  ⟩

public instance [HAdd α β γ] : HAdd (Vector3 α) (Vector3 β) (Vector3 γ) where
  hAdd := componentWiseAdd

public instance [HAdd α β γ] : HAdd (Vector3 β) (Vector3 α) (Vector3 γ) where
  hAdd lhs rhs := rhs.componentWiseAdd lhs

public instance [Neg α] : Neg (Vector3 α) where
  neg self := self.map (-·)

public instance [HSub α β γ] : HSub (Vector3 α) (Vector3 β) (Vector3 γ) where
  hSub := componentWiseSubtract

public instance [HMul α β γ] : HMul (Vector3 α) (β) (Vector3 γ) where
  hMul := scalarMultiply

public instance [HMul α β γ] : HMul (β) (Vector3 α) (Vector3 γ) where
  hMul c v := v.scalarMultiply c

public instance [HDiv α β γ] : HDiv (Vector3 α) (β) (Vector3 γ) where
  hDiv v c := v.scalarDivide c

end Vector3

end vector3
