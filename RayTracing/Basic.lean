import RayTracing.Netpbm
import RayTracing.Resolution

public def generateImage (header : Header) (pixelColor : (Nat × Nat) → Rgb8) : ByteArray :=
  let pixelsNetpbmBytes := header.resolution.pixelIndexes.flatMap ((·.toNetpbmBytes header) ∘ pixelColor)
  header.toNetpbmBytes ++ ⟨pixelsNetpbmBytes⟩
