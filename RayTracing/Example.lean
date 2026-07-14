import RayTracing.Resolution
import RayTracing.Netpbm
import RayTracing.Basic
import RayTracing.Ray

open Color

def outputPathRoot := "./output/"
def outputPathExtension := ".ppm"

def Scalar := Float
def Vector3 := Vector Scalar 3
def Ray' := Ray Scalar

namespace RedGreenGradient

def resolution : Resolution := {
  rowCount := 400
  columnCount := 400
}

def header : Header := {
  format := .P6
  resolution := resolution
  maxValue := 255
}

instance : Coe Float UInt8 where coe := (·.toUInt8)
instance : Coe UInt8 Float where coe := (·.toFloat)
def pixelColor (resolution : Resolution) (index : Nat × Nat) : Rgb8 :=
  let (rowIndex, columnIndex) := index

  let horizontalRatio := rowIndex.toFloat / resolution.rowCount.toFloat
  let verticalRatio := columnIndex.toFloat / resolution.columnCount.toFloat

  let color := rgb horizontalRatio verticalRatio 0.0
  let color := color.percentOf header.maxValue.toUInt8
  color

def imageData := generateImage header (pixelColor resolution)

def outputPath := s!"{outputPathRoot}red_green_gradient{outputPathExtension}"

#eval IO.FS.writeBinFile outputPath imageData

end RedGreenGradient
