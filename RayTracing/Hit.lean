import RayTracing.Vector3
import RayTracing.Ray

namespace hit

open vector3
open ray

namespace hit_record

public structure HitRecord α where
  position : Vector3 α
  normal : Vector3 α
  t : α

end hit_record

namespace hittable

open hit_record in
public class Hittable (Self Scalar : Type u) where
  hit (self : Self) (ray : Ray Scalar) (rayTMin rayTMax : Scalar) : Option (HitRecord Scalar)

end hittable

end hit
