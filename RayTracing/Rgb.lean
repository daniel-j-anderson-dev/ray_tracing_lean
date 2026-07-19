import RayTracing.Basic
import RayTracing.Vector3

namespace rgb

open vector3

public abbrev Rgb := Vector3
public abbrev vector3.Vector3.red := @Vector3.x
public abbrev vector3.Vector3.green := @Vector3.y
public abbrev vector3.Vector3.blue := @Vector3.z

end rgb
