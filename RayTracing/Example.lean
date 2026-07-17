import RayTracing.Resolution
import RayTracing.Netpbm
import RayTracing.Basic
import RayTracing.Ray
import RayTracing.Camera
import RayTracing.Viewport

open rgb
open resolution
open uint8_max

abbrev Scalar := Float
abbrev Vector3 := vector3.Vector3 Scalar
abbrev Ray := ray.Ray Scalar
abbrev Camera := camera.Camera Scalar
abbrev Viewport := viewport.Viewport Scalar
abbrev Viewport.mk' := viewport.Viewport.mk' (α := Scalar) (NatToα := ⟨Nat.toFloat⟩)

def outputPathRoot := "./output/"
def outputPathExtension := ".ppm"

namespace red_green_gradient

def header : netpbm.Header := {
  format := .P6
  resolution := {
    rowCount := 400
    columnCount := 400
  }
  maxValue := 255
}

def pixelColor
  (resolution : Resolution) (index : Nat × Nat)
  : Rgb UInt8 :=
  let (rowIndex, columnIndex) := index

  let horizontalRatio := rowIndex.toFloat / resolution.rowCount.toFloat
  let verticalRatio := columnIndex.toFloat / resolution.columnCount.toFloat

  let percentColor : Rgb _ := ⟨horizontalRatio, verticalRatio, 0.0⟩
  let color := percentColor * 255.0
  let color := color.map (·.toUInt8)
  color

def imageData := netpbm.generateImage header (pixelColor header.resolution)

def outputPath := s!"{outputPathRoot}red_green_gradient{outputPathExtension}"

#eval IO.FS.writeBinFile outputPath imageData

end red_green_gradient

namespace ray_cast


-- image

def idealAspectRatio := mkRat 16 9
def imageWidth := 400
def resolution := Resolution.fromColumnCountIdealAspectRatio imageWidth idealAspectRatio

def camera : Camera := {
  center := ⟨0,0,0⟩
  orientation := ⟨0, 0, 0, 0⟩
}

def viewport := Viewport.mk' (focalLength := 1.0) (height := 2.0) camera resolution

end ray_cast
