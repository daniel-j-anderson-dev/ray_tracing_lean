import RayTracing.Resolution
import RayTracing.Netpbm
import RayTracing.Basic
import RayTracing.Ray

open rgb
open resolution
open uint8_max

def outputPathRoot := "./output/"
def outputPathExtension := ".ppm"

def Scalar := Float
def Vector3 := vector3.Vector3 Scalar
def Ray := ray.Ray Scalar

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

  let color : Rgb _ := ⟨horizontalRatio, verticalRatio, 0.0⟩
  let color := color / UInt8.max.toFloat
  let color := color.map Float.toUInt8
  color

def imageData := netpbm.generateImage header (pixelColor header.resolution)

def outputPath := s!"{outputPathRoot}red_green_gradient{outputPathExtension}"

#eval IO.FS.writeBinFile outputPath imageData

end red_green_gradient
