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

def mk'
  [Mul α] [Add α] [Zero α] [Div α] [Neg α] [LT α] [DecidableRel (LT.lt (α := α))]
  (position : Vector3 α) (ray : Ray α) (t : α) (outwardNormal : Vector3 α)
  : HitRecord α :=
  let frontFace := ray.direction.dotProduct outwardNormal < 0
  let normal := if frontFace then outwardNormal else -outwardNormal
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

public instance : Hittable (List (AnyHittable β)) β where
  hit items ray tMin tMax := do
    let mut output := none
    let mut closestSoFar := tMax
    for item in items do
      let hit := item.hit ray tMin closestSoFar
      match hit with
      | none => continue
      | some hitRecord =>
        closestSoFar := hitRecord.t
        output := some hitRecord
    output

end AnyHittable

end any_hittable

end hit
