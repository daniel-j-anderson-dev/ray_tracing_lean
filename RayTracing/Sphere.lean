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
    -- vec3 oc = center - r.origin();
    -- auto a = r.direction().length_squared();
    -- auto h = dot(r.direction(), oc);
    -- auto c = oc.length_squared() - radius*radius;
    let offsetCenter := sphere.center - ray.origin
    let a := ray.direction.normSquared
    let h := ray.direction.dotProduct offsetCenter
    let c := offsetCenter.normSquared - (sphere.radius * sphere.radius)

    -- auto discriminant = h*h - a*c;
    -- if (discriminant < 0)
    --     return false;
    let discriminant := h * h - a * c
    if discriminant < 0 then none

    -- auto sqrtd = std::sqrt(discriminant);
    let sqrtDiscriminant := sqrt discriminant

    -- // Find the nearest root that lies in the acceptable range.
    -- auto root = (h - sqrtd) / a;
    -- if (root <= ray_tmin || ray_tmax <= root) {
    --     root = (h + sqrtd) / a;
    --     if (root <= ray_tmin || ray_tmax <= root)
    --         return false;
    -- }
    let root0 := (h - sqrtDiscriminant) / a
    let root1 := (h + sqrtDiscriminant) / a
    let inBounds x := rayTMin ≤ x ∧ x ≤ rayTMax
    let root ←
      if inBounds root0 then pure root0
      else if inBounds root1 then pure root1
      else none

    -- rec.t = root;
    -- rec.p = r.at(rec.t);
    -- rec.normal = (rec.p - center) / radius;
    -- return true;
    let hitPosition := ray.pointAt root
    let hitNormal := (hitPosition - sphere.center) / sphere.radius

    -- vec3 outward_normal = (rec.p - center) / radius;
    -- rec.set_face_normal(r, outward_normal);
    let hitRecord := HitRecord.mk' hitPosition ray (t := root) (outwardNormal := hitNormal)
    pure hitRecord

end Sphere

end sphere
