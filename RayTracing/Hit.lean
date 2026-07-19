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

namespace HitRecord

public instance [Inhabited α] : Inhabited (HitRecord α) where
  default :=
  let a: α := Inhabited.default
  {
    position := ⟨a, a, a⟩
    normal := ⟨a, a, a⟩
    t := a
  }

end HitRecord
end hit_record
namespace hittable

open hit_record in
public class Hittable α extends (Inhabited α) where
  hit (ray : Ray α) (rayTMin rayTMax : α) : Option (HitRecord α)

end hittable
end hit
