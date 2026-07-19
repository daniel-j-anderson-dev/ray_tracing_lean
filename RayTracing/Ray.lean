import RayTracing.Vector3
import RayTracing.Viewport
import RayTracing.Camera

namespace ray

open vector3
open camera
open viewport
open ray

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

end Ray


end ray
