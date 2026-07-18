import RayTracing.Basic

namespace rgb

open clamp renaming clamp → clampScalar

public structure Rgb α where
  red : α
  green : α
  blue : α

namespace Rgb

public def map
  (f : α → β) (self : Rgb α)
  : Rgb β :=
  ⟨f self.red, f self.green, f self.blue⟩

public def zipWith
  (f : α → β → γ) (self : Rgb α) (other : Rgb β)
  : Rgb γ :=
  ⟨f self.red other.red, f self.green other.green, f self.blue other.blue⟩

public def sumComponents
  [Add α]
  (self : Rgb α)
  : α :=
  self.red + self.green + self.blue

public def scalarMultiply
  [HMul α β γ]
  (self : Rgb α) (scalar : β)
  : Rgb γ :=
  self.map (· * scalar)

public def scalarDivide
  [HDiv α β γ]
  (self : Rgb α) (scalar : β)
  : Rgb γ :=
  self.map (· / scalar)

public def componentWiseAdd
  [HAdd α β γ]
  (lhs : Rgb α) (rhs : Rgb β)
  : Rgb γ :=
  lhs.zipWith (· + ·) rhs

public def componentWiseSubtract
  [HSub α β γ]
  (lhs : Rgb α) (rhs : Rgb β)
  : Rgb γ :=
  lhs.zipWith (· - ·) rhs

public def componentWiseMultiply
  [HMul α β γ]
  (lhs : Rgb α) (rhs : Rgb β)
  : Rgb γ :=
  lhs.zipWith (· * ·) rhs

public def clamp
  [Max α] [Min α]
  (self : Rgb α) (lower upper: α)
  : Rgb α :=
  self.map (clampScalar · lower upper)

public def toArray
  (self : Rgb α)
  : Array α :=
  #[self.red, self.green, self.blue]

public def toByteArray
  (self : Rgb UInt8)
  : ByteArray :=
  ⟨self.toArray⟩

public def toVector
  (self : Rgb α)
  : Vector α 3 :=
  ⟨self.toArray, rfl⟩

public instance [HAdd α β γ] : HAdd (Rgb α) (Rgb β) (Rgb γ) where
  hAdd := componentWiseAdd

public instance [HAdd α β γ] : HAdd (Rgb β) (Rgb α) (Rgb γ) where
  hAdd lhs rhs := rhs.componentWiseAdd lhs

public instance [Neg α] : Neg (Rgb α) where
  neg self := self.map (-·)

public instance [HSub α β γ] : HSub (Rgb α) (Rgb β) (Rgb γ) where
  hSub := componentWiseSubtract

public instance [HMul α β γ] : HMul (Rgb α) (β) (Rgb γ) where
  hMul := scalarMultiply

public instance [HMul α β γ] : HMul (β) (Rgb α) (Rgb γ) where
  hMul c v := v.scalarMultiply c

public instance [HDiv α β γ] : HDiv (Rgb α) (β) (Rgb γ) where
  hDiv v c := v.scalarDivide c

public instance ToString [ToString α] : ToString (Rgb α) where
  toString self := s!"{self.red}, {self.green}, {self.blue}"

public instance [Zero α] : Zero (Rgb α) where
  zero := ⟨0, 0, 0⟩

public instance [One α] : One (Rgb α) where
  one := ⟨1, 1, 1⟩

public instance [Coe α β] : Coe (Rgb α) (Rgb β) where
  coe := Rgb.map (f := Coe.coe)

end Rgb

end rgb
