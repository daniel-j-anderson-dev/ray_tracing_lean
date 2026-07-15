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

public def Format.encoding : Format → Encoding
  | .P1 | .P2 | .P3 => .ascii
  | .P4 | .P5 | .P6 => .binary

public structure Header where
  resolution : Resolution
  format : Format
  maxValue : Nat

public instance : ToString Header where
  toString header :=
    s!"{header.format}\n{header.resolution.columnCount} {header.resolution.rowCount}\n{header.maxValue}\n"

public def Header.toNetpbmBytes (header : Header) : ByteArray :=
  s!"{header}".toByteArray

public def Rgb.toNetpbmBytes
  [ToString α] [Max α] [Min α] [Zero α] [Coe α UInt8] [Coe Nat α]
  (color : Rgb α) (header : Header)
  : Array UInt8 :=
  let clamped := (color.clamp 0 header.maxValue)
  match header.format.encoding with
  | .binary => clamped.toArray.map Coe.coe
  | .ascii => s!"{clamped.red} {clamped.green} {clamped.blue}\n".toByteArray.data

public instance : Coe Nat UInt8 where coe := (·.toUInt8)
public instance : Coe α α where coe := id
public def generateImage
  (header : Header) (pixelColor : Nat × Nat → Rgb UInt8) : ByteArray :=
  let pixelsNetpbmBytes := header.resolution.pixelIndexes.flatMap ((Rgb.toNetpbmBytes · header) ∘ pixelColor)
  header.toNetpbmBytes ++ ⟨pixelsNetpbmBytes⟩

end netpbm
