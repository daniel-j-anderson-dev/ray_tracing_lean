import RayTracing.Vector3
import RayTracing.Hit
import RayTracing.Ray
import RayTracing.Rgb

namespace sphere

open sqrt
open vector3
open ray
open hit
open hittable
open hit_record
open rgb

public structure Sphere α where
  center : Vector3 α
  radius : α

namespace Sphere

public instance
  [One α] [Zero α] [LT α] [LE α] [Add α] [Sub α] [Neg α] [Mul α] [Div α] [Sqrt α]
  [DecidableRel (LT.lt (α := α))]
  [DecidableRel (LE.le (α := α))]
  : Hittable (Sphere α) α where
  hit sphere ray rayTMin rayTMax : Option (HitRecord α) := do
    let offsetCenter := sphere.center - ray.origin
    let a := ray.direction.normSquared
    let h := ray.direction.dotProduct offsetCenter
    let c := offsetCenter.normSquared - (sphere.radius * sphere.radius)

    let discriminant := h * h - a * c
    if discriminant < 0 then none
    let sqrtDiscriminant := sqrt discriminant

    let root0 := (h - sqrtDiscriminant) / a
    let root1 := (h + sqrtDiscriminant) / a
    let inBounds x := rayTMin ≤ x ∧ x ≤ rayTMax
    let root ←
      if inBounds root0 then pure root0
      else if inBounds root1 then pure root1
      else none

    let hitPosition := ray.pointAt root
    let hitNormal := (hitPosition - sphere.center) / sphere.radius

    let hitRecord := HitRecord.mk' hitPosition ray (t := root) (outwardNormal := hitNormal)
    pure hitRecord

end Sphere

end sphere
