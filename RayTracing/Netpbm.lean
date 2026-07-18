import RayTracing.Basic
import RayTracing.Resolution
import RayTracing.Rgb

open rgb
open resolution

namespace netpbm


public inductive Format where
  | P1
  | P2
  | P3
  | P4
  | P5
  | P6
  deriving Repr

public instance : ToString Format where
  toString
  | .P1 => "P1"
  | .P2 => "P2"
  | .P3 => "P3"
  | .P4 => "P4"
  | .P5 => "P5"
  | .P6 => "P6"

public inductive Encoding where
| ascii
| binary
  deriving Repr

public def Format.encoding : Format → Encoding
  | .P1 | .P2 | .P3 => .ascii
  | .P4 | .P5 | .P6 => .binary

public structure Header where
  resolution : Resolution
  format : Format
  maxValue : Nat
  deriving Repr

public instance : ToString Header where
  toString header :=
    s!"{header.format}\n{header.resolution.columnCount} {header.resolution.rowCount}\n{header.maxValue}\n"

public def Header.toNetpbmBytes (header : Header) : ByteArray :=
  s!"{header}".toByteArray

public def Rgb.toNetpbmBytes
  [ToString α] [Max α] [Min α] [Zero α] [αToUInt8 : Coe α UInt8] [NatToα : Coe Nat α]
  (header : Header) (color : Rgb α)
  : Array UInt8 :=
  let clamped := (color.clamp 0 header.maxValue)
  match header.format.encoding with
  | .binary => clamped.toArray.map Coe.coe
  | .ascii => s!"{clamped.red} {clamped.green} {clamped.blue}\n".toByteArray.data

public def generateImage
  (header : Header) (pixelColor : Nat × Nat → Rgb UInt8)
  : ByteArray :=
  let colorToNetpbmBytes := Rgb.toNetpbmBytes (αToUInt8 := ⟨id⟩) (NatToα := ⟨Nat.toUInt8⟩) header
  let pixelNetpbmColor := (colorToNetpbmBytes ∘ pixelColor)
  let pixelNetpbmBytes := ⟨header.resolution.pixelIndexes.flatMap pixelNetpbmColor⟩
  let image := header.toNetpbmBytes ++ pixelNetpbmBytes
  image

end netpbm
