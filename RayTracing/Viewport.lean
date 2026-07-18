import RayTracing.Vector3
import RayTracing.Camera
import RayTracing.Resolution

namespace viewport

open resolution
open vector3
open camera

public structure Viewport α where
  --- The center of pixel at row 0, column 0
  topLeft : Vector3 α

  --- The "horizontal" change between pixels
  Δx : Vector3 α

  --- The "vertical" change between pixels
  Δy : Vector3 α

namespace Viewport

public instance ToString [ToString α] : ToString (Viewport α) where
  toString self := s!"\{topLeft:= {self.topLeft}, Δx:= {self.Δx}, Δy:= {self.Δy}}"

public def mk'
  [Add α] [Sub α] [Neg α] [Mul α] [Div α] [Zero α] [One α]
  [NatToα : Coe Nat α]
  (focalLength : α) (height : α) (camera : Camera α) (resolution : Resolution)
  : Viewport α :=
  let two : α := 1 + 1

  let width := height * resolution.aspectRatio

  let x := camera.right * width
  let y := camera.up * -height
  let z := camera.forward * focalLength

  let center := camera.center + z
  let topLeft := center - ((x + y) / two)

  let pixelΔX := x / (resolution.columnCount : α)
  let pixelΔY := y / (resolution.rowCount : α)
  let offset := (pixelΔX + pixelΔY) / two
  let pixelTopLeft := topLeft + offset

  ⟨pixelTopLeft, pixelΔX, pixelΔY⟩

end Viewport

end viewport
