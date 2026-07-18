import RayTracing.Resolution
import RayTracing.Netpbm
import RayTracing.Basic
import RayTracing.Ray
import RayTracing.Camera
import RayTracing.Viewport
import RayTracing.Sphere

open rgb
open resolution
open uint8_max
open sqrt

abbrev Scalar := Float
abbrev Byte := UInt8
abbrev Byte.max : Byte := 255
local instance : Coe Nat Scalar := ⟨Nat.toFloat⟩
abbrev Vector3 := vector3.Vector3 Scalar
abbrev Ray := ray.Ray Scalar
abbrev Camera := camera.Camera Scalar
abbrev Viewport := viewport.Viewport Scalar
abbrev Viewport.mk' := viewport.Viewport.mk' (α := Scalar)
abbrev rayCast := @ray.rayCast
abbrev Sphere := sphere.Sphere Scalar

def outputPathRoot := "./output/"
def outputPathExtension := ".ppm"

abbrev vector3.Vector3.toRgb
  (self : vector3.Vector3 α)
  : Rgb α :=
  ⟨self.x, self.y, self.z⟩

abbrev Rgb.toVector3
  (self : Rgb α)
  : vector3.Vector3 α :=
  ⟨self.red, self.green, self.blue⟩

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
  : Rgb Byte :=
  let (rowIndex, columnIndex) := index

  let horizontalRatio := rowIndex.toFloat / resolution.rowCount.toFloat
  let verticalRatio := columnIndex.toFloat / resolution.columnCount.toFloat

  let percentColor : Rgb _ := ⟨horizontalRatio, verticalRatio, 0.0⟩
  let color := percentColor.map ((·.toUInt8) ∘ (· * 255.0))
  color

def imageData := netpbm.generateImage header (pixelColor header.resolution)

def outputPath := s!"{outputPathRoot}red_green_gradient{outputPathExtension}"

#eval IO.FS.writeBinFile outputPath imageData

end red_green_gradient

namespace ray_cast

def idealAspectRatio := mkRat 16 9
def imageWidth := 400
def resolution := Resolution.fromColumnCountIdealAspectRatio imageWidth idealAspectRatio

def header := { red_green_gradient.header with resolution := resolution }
def outputPath := s!"{outputPathRoot}ray_cast.csv"

def camera : Camera := {
  center := ⟨0,0,0⟩
  orientation := ⟨0, 0, 0, 1⟩
}

def focalLength := 1.0
def viewportHeight := 2.0
def viewport := Viewport.mk' focalLength viewportHeight camera resolution

-- https://www.desmos.com/3d/bsf4pxj1tz
#eval idealAspectRatio
#eval imageWidth
#eval resolution
#eval resolution.aspectRatio (α := Scalar)
#eval camera.center
#eval camera.right
#eval camera.up
#eval camera.forward
#eval focalLength
#eval viewportHeight
#eval viewport
#eval rayCast camera viewport (resolution.deserializePixelIndex 1225)

def rayCastCsv :=
  let rays := resolution.pixelIndexes.map (rayCast camera viewport)
  let csvColumnNames := "pixelIndex, ray.origin.x, ray.origin.y, ray.origin.z, ray.direction.x, ray.direction.y, ray.direction.z\n"
  let raysCsv := rays.toList.zipIdx.map λ (ray, i) => s!"{i}, {ray.origin.x}, {ray.origin.y}, {ray.origin.z}, {ray.direction.x}, {ray.direction.y}, {ray.direction.z}\n"
  let raysCsv := raysCsv.foldl (· ++ ·) ""
  csvColumnNames ++ raysCsv

-- #eval IO.FS.writeFile outputPath rayCastCsv

end ray_cast

namespace blue_white_gradient

def resolution := ray_cast.resolution
def viewport := ray_cast.viewport
def camera := ray_cast.camera
def header := ray_cast.header

def rayColor
  (ray : Ray)
  : Rgb Scalar :=
  let unitDirection := ray.direction.normalize.getD 0
  let a := (unitDirection.y + 1) / 2
  let startColor : Rgb _ := ⟨1.0, 1.0, 1.0⟩
  let endColor : Rgb _ := ⟨0.5, 0.7, 1.0⟩
  let percentColor := (1 - a) * startColor + a * endColor
  percentColor

def pixelColor
  (index : Nat × Nat)
  : Rgb Byte :=
  let ray := rayCast camera viewport index
  let percentColor := rayColor ray
  let color := percentColor.map (Float.toUInt8 ∘ (· * 255))
  color

def image := netpbm.generateImage header pixelColor

def outputPath := s!"{outputPathRoot}blue_white_gradient{outputPathExtension}"

#eval IO.FS.writeBinFile outputPath image

end blue_white_gradient

namespace sphere_no_shading

def resolution := ray_cast.resolution
def viewport := ray_cast.viewport
def camera := ray_cast.camera
def header := ray_cast.header

def raySphereIntersection
  (ray : Ray) (sphere : Sphere)
  : Option Scalar :=
  let offsetCenter := sphere.center - ray.origin
  let a := ray.direction.normSquared
  let b := -2 * ray.direction.dotProduct offsetCenter
  let c := offsetCenter.normSquared - (sphere.radius * sphere.radius)
  let discriminant := b*b - 4*a*c
  if discriminant < 0 then
    none
  else
    some ((-b - (sqrt discriminant)) / (2 * a))

def pixelColor
  (sphere : Sphere) (pixelIndex : Nat × Nat)
  : Rgb Byte :=
  let ray := rayCast camera viewport pixelIndex
  match raySphereIntersection ray sphere with
  | none => blue_white_gradient.pixelColor pixelIndex
  | some _ => ⟨255, 0, 0⟩

def sphere : Sphere := {
  center := ⟨0, 0, -1⟩,
  radius := 0.5
}

def image := netpbm.generateImage header (pixelColor sphere)

def outputPath := s!"{outputPathRoot}sphere_no_shading{outputPathExtension}"

#eval IO.FS.writeBinFile outputPath image

end sphere_no_shading

namespace sphere_surface_normals

def resolution := ray_cast.resolution
def viewport := ray_cast.viewport
def camera := ray_cast.camera
def header := ray_cast.header

def raySphereIntersection
  (ray : Ray) (sphere : Sphere)
  : Option Scalar :=
  let offsetCenter := sphere.center - ray.origin
  let a := ray.direction.normSquared
  let h := ray.direction.dotProduct offsetCenter
  let c := offsetCenter.normSquared - (sphere.radius * sphere.radius)
  let discriminant := (h * h) - (a * c)
  if discriminant < 0 then
    none
  else
    some ((h - sqrt discriminant) / a)

def rayColor
  (ray : Ray) (sphere : Sphere)
  : Rgb Scalar :=
  Option.getD (
    do
    let t ← raySphereIntersection ray sphere
    let _ ← if t == 0 then none else some ()
    let hitPoint := ray.pointAt t
    let normal := (hitPoint - sphere.center).normalize.getD 0
    let normalColor := (normal.toRgb + 1) / 2.0
    some normalColor
  )
 (blue_white_gradient.rayColor ray)


def pixelColor
  (sphere : Sphere) (pixelIndex : Nat × Nat)
  : Rgb Byte :=
  let ray := rayCast camera viewport pixelIndex
  let color := rayColor ray sphere
  let color := color.map (Float.toUInt8 ∘ (· * 255))
  color

def sphere : Sphere := {
  center := ⟨0, 0, -1⟩,
  radius := 0.5
}

def image := netpbm.generateImage header (pixelColor sphere)

def outputPath := s!"{outputPathRoot}sphere_surface_normals{outputPathExtension}"

#eval IO.FS.writeBinFile outputPath image

end sphere_surface_normals
