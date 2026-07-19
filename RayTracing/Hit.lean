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

open hit_record in
public class Hittable (Self Scalar : Type u) where
  hit (self : Self) (ray : Ray Scalar) (rayTMin rayTMax : Scalar) : Option (HitRecord Scalar)

end hittable

end hit
