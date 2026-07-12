import RayTracing

def resolution : Resolution := {
  rowCount := 400
  columnCount := 400
}

def header : Header := {
  format := .P6
  resolution := resolution
  maxValue := 255
}

def pixelColor (resolution : Resolution) (index : Nat × Nat) : Rgb8 :=
  let (rowIndex, columnIndex) := index

  let horizontalRatio := rowIndex.toFloat / resolution.rowCount.toFloat
  let verticalRatio := columnIndex.toFloat / resolution.columnCount.toFloat

  let color : RgbPercent := (horizontalRatio, verticalRatio, 0.0)
  let color := color.percentOf header.maxValue.toUInt8
  color

def imageData := generateImage header (pixelColor resolution)

def outputPath := "output.ppm"

def saveImageData := IO.FS.writeBinFile outputPath imageData

def main := saveImageData
