import RayTracing.Resolution

public inductive Format where
| P1
| P2
| P3
| P4
| P5
| P6

public instance : ToString Format where
  toString format :=
    match format with
    | .P1 => "P1"
    | .P2 => "P2"
    | .P3 => "P3"
    | .P4 => "P4"
    | .P5 => "P5"
    | .P6 => "P6"

public inductive Encoding where
| ascii
| binary

public def Format.encoding (format : Format) : Encoding :=
  match format with
  | .P1 | .P2 | .P3 => .ascii
  | .P4 | .P5 | .P6 => .binary

public structure Header where
  resolution : Resolution
  format : Format
  maxValue : Nat

public instance : ToString Header where
  toString header := s!"{header.format}\n{header.resolution.columnCount} {header.resolution.rowCount}\n{header.maxValue}\n"

public def Header.toNetpbmBytes (header : Header) : ByteArray := s!"{header}".toByteArray

public abbrev clamp' [Max α] [Min α] (value : α) (lower : α) (upper : α) : α :=
  max lower (min value upper)

public abbrev RgbPercent := Vector Float 3

public instance : Coe (α × α × α) (Vector α 3) where
  coe
  | (x0, x1, x2) => ⟨#[x0, x1, x2], rfl⟩

public abbrev Rgb8 := Vector UInt8 3

public abbrev Vector.red (color : Vector α 3) :=
  color.get ⟨0, by decide⟩

public abbrev Vector.green (color : Vector α 3) :=
  color.get ⟨1, by decide⟩

public abbrev Vector.blue (color : Vector α 3) :=
  color.get ⟨2, by decide⟩

public abbrev Vector.clamp
  [Max α] [Min α]
  (color : Vector α n) (lower : α) (upper : α)
  : Vector α n :=
  color.map (clamp' · lower upper)

public instance : Coe Float UInt8 where coe := (·.toUInt8)
public instance : Coe UInt8 Float where coe := (·.toFloat)
public abbrev Vector.percentOf
  [Max α] [Min α] [Zero α] [Coe α β] [Coe β α] [Mul α]
  (self : Vector α n) (value : β)
  : Vector β n :=
  let clamped := self.clamp 0 value
  let scaled := clamped.map (· * (value : α))
  let casted := scaled.map (Coe.coe ·)
  casted

public instance : Coe Nat UInt8 where coe := (·.toUInt8)
public instance : Coe α α where coe := id
public abbrev Vector.toNetpbmBytes
  [ToString α] [Max α] [Min α] [Zero α] [Coe Nat α] [Coe α UInt8]
  (color : Vector α 3) (header : Header)
  : Array UInt8 :=
  let clamped := (color.clamp 0 header.maxValue)
  match header.format.encoding with
  | .binary => clamped.toArray.map Coe.coe
  | .ascii => s!"{clamped.red} {clamped.green} {clamped.blue}\n".toByteArray.data
