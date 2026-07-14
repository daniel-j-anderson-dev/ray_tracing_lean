namespace Sqrt

public class Sqrt α where
  sqrt : α → α

public instance Float32.Sqrt : Sqrt Float32 where
  sqrt := Float32.sqrt

public instance Float.Sqrt : Sqrt Float where
  sqrt := Float.sqrt

public def sqrt [s : Sqrt α] (x : α) : α := s.sqrt x

end Sqrt

namespace Clamp

public def clamp
  [Max α] [Min α]
  (value : α) (lower : α) (upper : α)
  : α :=
  max lower (min value upper)

end Clamp

namespace Color

public abbrev Rgb α := Vector α 3

public abbrev RgbPercent := Rgb Float

public abbrev Rgb8 := Rgb UInt8

public def rgb (red green blue : α) : Rgb α :=
  ⟨#[red, green, blue], rfl⟩

public def Vector.red (color : Vector α 3) :=
  color.get ⟨0, by decide⟩

public def Vector.green (color : Vector α 3) :=
  color.get ⟨1, by decide⟩

public def Vector.blue (color : Vector α 3) :=
  color.get ⟨2, by decide⟩

open Clamp renaming clamp → clampScalar in
public def Vector.clamp
  [Max α] [Min α]
  (color : Vector α n) (lower upper: α)
  : Vector α n :=
  color.map (clampScalar · lower upper)

open Color in
public def Vector.percentOf
  [Max α] [Min α] [Zero α] [Coe α β] [Coe β α] [Mul α]
  (self : Vector α n) (value : β)
  : Vector β n :=
  let clamped := self.clamp 0 value
  let scaled := clamped.map (· * (value : α))
  let casted := scaled.map (Coe.coe ·)
  casted

end Color
