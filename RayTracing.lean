import RayTracing.Resolution
import RayTracing.Netpbm

public def generateImage (header : Header) (pixelColor : (Nat × Nat) → Rgb8) : ByteArray :=
  let pixelsNetpbmBytes := header.resolution.pixelIndexes.flatMap ((·.toNetpbmBytes header) ∘ pixelColor)
  header.toNetpbmBytes ++ ⟨pixelsNetpbmBytes⟩
