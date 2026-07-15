import RayTracing.Vector3
import RayTracing.Quaternion

namespace camera

open vector3
open quaternion

public structure Camera α where
  center: Vector3 α
  orientation : Quaternion α

namespace Camera

public def right
  [Mul α] [Add α] [Sub α] [Zero α] [One α]
  (self : Camera α)
  : Vector3 α :=
  self.orientation.xAxis

public def up
  [Mul α] [Add α] [Sub α] [Zero α] [One α]
  (self : Camera α)
  : Vector3 α :=
  self.orientation.yAxis

public def forward
  [Mul α] [Add α] [Sub α] [Zero α] [One α]
  (self : Camera α)
  : Vector3 α :=
  self.orientation.zAxis

end Camera

end camera
