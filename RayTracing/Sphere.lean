import RayTracing.Vector3
import RayTracing.Ray
import RayTracing.Rgb

namespace sphere

open sqrt
open vector3
open ray
open rgb

public structure Sphere α where
  center : Vector3 α
  radius : α

namespace Sphere

public instance Ray.Intersect
  [One α] [Zero α]
  [Add α] [Sub α] [Neg α] [Mul α] [Div α]
  [LT α] [LE α] [DecidableLT α] [DecidableLE α]
  [Sqrt α]
  : Ray.Intersect (Sphere α) α where
  intersect sphere ray rayT := do
    let offsetCenter := sphere.center - ray.origin
    let a := ray.direction.normSquared
    let h := ray.direction.dotProduct offsetCenter
    let c := offsetCenter.normSquared - (sphere.radius * sphere.radius)

    let discriminant := h * h - a * c
    if discriminant < 0 then none
    let sqrtDiscriminant := sqrt discriminant

    let root₀ := (h - sqrtDiscriminant) / a
    let root₁ := (h + sqrtDiscriminant) / a
    let root ←
      if rayT.contains root₀ then pure root₀
      else if rayT.contains root₁ then pure root₁
      else none

    let hitPosition := ray.pointAt root
    let hitNormal := (hitPosition - sphere.center) / sphere.radius

    let hitRecord := Ray.Intersection.mk' hitPosition ray (t := root) (outwardNormal := hitNormal)
    pure hitRecord

end Sphere

end sphere
