import RayTracing.VectorUtil

public structure Ray α where
  origin : Vector α 3
  direction : Vector α 3

public def Ray.at
  [HMul α τ β] [HAdd α β γ]
  (self : Ray α) (t : τ)
  : Vector γ 3 :=
  self.origin + t * self.direction
