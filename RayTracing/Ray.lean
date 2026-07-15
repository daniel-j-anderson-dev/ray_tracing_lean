import RayTracing.Vector3

namespace ray

open vector3

public structure Ray α where
  origin : Vector3 α
  direction : Vector3 α

namespace Ray

public def pointAt
  [HMul α τ β] [HAdd α β γ]
  (self : Ray α) (t : τ)
  : Vector3 γ :=
  self.origin + t * self.direction

end Ray


end ray
