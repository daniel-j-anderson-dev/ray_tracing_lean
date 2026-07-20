import RayTracing.Vector3
import RayTracing.Ray

namespace hit

open vector3
open ray

namespace hit_record

public structure HitRecord α where
  private mk ::
  position : Vector3 α
  normal : Vector3 α
  t : α

namespace HitRecord

-- pub fn init(args: struct {
--     position: Vector3,
--     t: Scalar,
--     ray: Ray,
--     outward_normal: Vector3,
-- }) Self {
-- }
def mk'
  [Mul α]
  [Add α]
  [Zero α]
  [Div α]
  [Neg α]
  [LT α]
  [DecidableRel (LT.lt (α := α))]
  (position : Vector3 α)
  (ray : Ray α)
  (t : α)
  (outwardNormal : Vector3 α)
  : HitRecord α :=
  -- const front_face = ray.direction.dotProduct(outward_normal) < 0;
  -- const normal = if (front_face) outward_normal else outward_normal.scalarMultiply(-1);
  let frontFace := ray.direction.dotProduct outwardNormal < 0
  let normal := if frontFace then outwardNormal else -outwardNormal

  -- return .{
  --     .position = position,
  --     .t = t,
  --     .front_face = front_face,
  --     .normal = normal,
  -- };
  {
    position := position
    t := t
    normal := normal
  }

end HitRecord

end hit_record

namespace hittable

open hit_record

/--
# Params
- `α`
  - The type that is "Hittable"
- `β`
  - the scalar type used in computation
-/
public class Hittable (α : Type u) (β : outParam (Type v)) where
  hit (self : α) (ray : Ray β) (rayTMin rayTMax : β) : Option (HitRecord β)

end hittable

namespace any_hittable

open hittable

public structure AnyHittable (β : Type v) where
  {Data : Type u}
  data : Data
  [vtable : Hittable Data β]

namespace AnyHittable

open hit_record

public def hit
  (self : AnyHittable β)
  (ray : Ray β)
  (tMin tMax : β)
  : Option (HitRecord β) :=
  self.vtable.hit self.data ray tMin tMax

public instance [Hittable α β] : CoeHead α (AnyHittable β) where
  coe := (⟨·⟩)

end AnyHittable

end any_hittable

end hit
