  import RayTracing.Basic
import RayTracing.Vector3

namespace quaternion

open sqrt
open vector3

public structure Quaternion α where
  x : α
  y : α
  z : α
  w : α

namespace Quaternion

public def map
  (f: α → β) (self : Quaternion α)
  : Quaternion β :=
  ⟨f self.x, f self.y, f self.z, f self.w⟩

public def zipWith
  (f : α → β → γ) (self : Quaternion α) (other : Quaternion β)
  : Quaternion γ :=
  ⟨f self.x other.x, f self.y other.y, f self.z other.z, f self.w other.w⟩

public def foldl
  (f : α → α → α) (init : α) (self : Quaternion α)
  : α :=
  let output := init
  let output := f output self.x
  let output := f output self.y
  let output := f output self.z
  let output := f output self.w
  output

public def sumComponents
  [Add α] [Zero α]
  (self : Quaternion α)
  : α :=
  self.foldl (· + ·) 0

public def scalarMultiply
  [HMul α β γ]
  (self : Quaternion α) (scalar : β)
  : Quaternion γ :=
  self.map (· * scalar)

public def scalarDivide
  [HDiv α β γ]
  (self : Quaternion α) (scalar : β)
  : Quaternion γ :=
  self.map (· / scalar)

public def componentWiseAdd
  [HAdd α β γ]
  (lhs : Quaternion α) (rhs : Quaternion β)
  : Quaternion γ :=
  lhs.zipWith (· + ·) rhs

public def componentWiseSubtract
  [HSub α β γ]
  (lhs : Quaternion α) (rhs : Quaternion β)
  : Quaternion γ :=
  lhs.zipWith (· - ·) rhs

public def componentWiseMultiply
  [HMul α β γ]
  (lhs : Quaternion α) (rhs : Quaternion β)
  : Quaternion γ :=
  lhs.zipWith (· * ·) rhs

public def dotProduct
  [HMul α β γ] [Add γ] [Zero γ]
  (lhs : Quaternion α) (rhs : Quaternion β)
  : γ :=
  (lhs.componentWiseMultiply rhs).sumComponents

public def normSquared
  [Mul α] [Add α] [Zero α]
  (self : Quaternion α)
  : α :=
  self.dotProduct self

public def norm
  [Mul α] [Zero α] [Add α] [Sqrt α]
  (self : Quaternion α)
  : α :=
  sqrt self.normSquared

public def normalize
  [Mul α] [Div α] [Zero α] [Add α] [Sqrt α] [BEq α]
  (self : Quaternion α)
  : Option (Quaternion α) := do
  let normSquared := self.normSquared
  let norm ← if normSquared == 0 then none else some (sqrt normSquared)
  self.scalarDivide norm

public def xAxis
  [Mul α] [Add α] [Sub α] [Zero α] [One α]
  (self : Quaternion α)
  : Vector3 α :=
  let ⟨x, y, z, w⟩ := self
  let y2 := y * y
  let z2 := z * z
  let two : α := 1 + 1
  ⟨
    1 - two * (y2 + z2),
    two * (x * y + w * z),
    two * (x * z + w * y),
  ⟩

public def yAxis
  [Mul α] [Add α] [Sub α] [Zero α] [One α]
  (self : Quaternion α)
  : Vector3 α :=
  let ⟨x, y, z, w⟩ := self
  let x2 := x * x
  let z2 := z * z
  let two : α := 1 + 1
  ⟨
    two * (x * y - w * z),
    1 - two * (x2 + z2),
    two * (y * z + w * x),
  ⟩

public def zAxis
  [Mul α] [Add α] [Sub α] [Zero α] [One α]
  (self : Quaternion α)
  : Vector3 α :=
  let ⟨x, y, z, w⟩ := self
  let y2 := y * y
  let x2 := x * x
  let two : α := 1 + 1
  ⟨
    two * (x * z + w * y),
    two * (y * z + w * x),
    1 - two * (x2 + y2),
  ⟩

public instance [HAdd α β γ] : HAdd (Quaternion α) (Quaternion β) (Quaternion γ) where
  hAdd := componentWiseAdd

public instance [HAdd α β γ] : HAdd (Quaternion β) (Quaternion α) (Quaternion γ) where
  hAdd lhs rhs := rhs.componentWiseAdd lhs

public instance [HMul α β γ] : HMul (Quaternion α) (β) (Quaternion γ) where
  hMul := scalarMultiply

public instance [HMul α β γ] : HMul (β) (Quaternion α) (Quaternion γ) where
  hMul c v := v.scalarMultiply c

public instance [HDiv α β γ] : HDiv (Quaternion α) (β) (Quaternion γ) where
  hDiv v c := v.scalarDivide c

public instance ToString [ToString α] : ToString (Quaternion α) where
  toString self := s!"{self.x}, {self.y}, {self.z}, {self.w}"

public instance [Zero α] : Zero (Quaternion α) where
  zero := ⟨0, 0, 0, 0⟩

public instance [One α] : One (Quaternion α) where
  one := ⟨1, 1, 1, 1⟩

end Quaternion

end quaternion
