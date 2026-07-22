import RayTracing.Vector3
import RayTracing.Viewport
import RayTracing.Camera
import RayTracing.Interval

namespace ray

open vector3
open camera
open viewport
open ray
open interval

public structure Ray α where
  origin : Vector3 α
  direction : Vector3 α

public def rayCast
  [NatToα : Coe Nat α] [Mul α] [Add α] [Sub α]
  (camera : Camera α) (viewport : Viewport α) (index : Nat × Nat)
  : Ray α :=
  let pixelCenter := viewport.pixelCenter index
  let pixelToCamera := pixelCenter - camera.center
  {
    origin := pixelCenter
    direction := pixelToCamera
  }

namespace Ray

public instance ToString [ToString α] : ToString (Ray α) where
  toString self := s!"\{origin := {self.origin}, direction := {self.direction}}"

public def pointAt
  [HMul α τ β] [HAdd α β γ]
  (self : Ray α) (t : τ)
  : Vector3 γ :=
  self.origin + t * self.direction

public structure Intersection α where
  private mk ::
  position : Vector3 α
  normal : Vector3 α
  t : α

namespace Intersection

def mk'
  [Mul α] [Add α] [Zero α] [Div α] [Neg α] [LT α] [DecidableRel (LT.lt (α := α))]
  (position : Vector3 α) (ray : Ray α) (t : α) (outwardNormal : Vector3 α)
  : Intersection α :=
  let frontFace := ray.direction.dotProduct outwardNormal < 0
  let normal := if frontFace then outwardNormal else -outwardNormal
  {
    position := position
    t := t
    normal := normal
  }

end Intersection

public class Intersect (α : Type u) (β : outParam (Type v)) where
  intersect (self : α) (ray : Ray β) (rayT : Interval β) : Option (Intersection β)

/--
An opaque wrapper around a type `Data` and a specific instance of `Intersect`. Used for runtime polymorphism
-/
public structure AnyIntersect (β : Type v) where
  {Data : Type u}
  data : Data
  [vtable : Intersect Data β]

namespace AnyIntersect

public def intersect
  (self : AnyIntersect β) (ray : Ray β) (rayT : Interval β)
  : Option (Intersection β) :=
  self.vtable.intersect self.data ray rayT

public instance CoeHead
  [Intersect α β]
  : CoeHead α (AnyIntersect β) where
  coe := (⟨·⟩)


public instance List.Intersect
  : Intersect (List (AnyIntersect β)) β where
  intersect items ray rayT := do
    let mut output := none
    let mut closestSoFar := rayT.max
    for item in items do
      match item.intersect ray ⟨rayT.min, closestSoFar⟩ with
      | none => continue
      | some intersection =>
        closestSoFar := intersection.t
        output := some intersection
    output

end AnyIntersect

end Ray


end ray
