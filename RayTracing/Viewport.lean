import RayTracing.Vector3
import RayTracing.Camera
import RayTracing.Resolution

namespace viewport

open resolution
open vector3
open camera

public structure Viewport α where
  --- The center of pixel at row 0, column 0
  topLeft : Vector3 α

  --- The "horizontal" change between pixels
  Δx : Vector3 α

  --- The "vertical" change between pixels
  Δy : Vector3 α

namespace Viewport

public instance ToString [ToString α] : ToString (Viewport α) where
  toString self := s!"\{topLeft:= {self.topLeft}, Δx:= {self.Δx}, Δy:= {self.Δy}}"


/-
```cpp
auto aspect_ratio = 16.0 / 9.0;
int image_width = 400;

// Calculate the image height, and ensure that it's at least 1.
int image_height = int(image_width / aspect_ratio);
image_height = (image_height < 1) ? 1 : image_height;

// Camera

auto focal_length = 1.0;
auto viewport_height = 2.0;
auto viewport_width = viewport_height * (double(image_width)/image_height);
auto camera_center = point3(0, 0, 0);

// Calculate the vectors across the horizontal and down the vertical viewport edges.
auto viewport_u = vec3(viewport_width, 0, 0);
auto viewport_v = vec3(0, -viewport_height, 0);

// Calculate the horizontal and vertical delta vectors from pixel to pixel.
auto pixel_delta_u = viewport_u / image_width;
auto pixel_delta_v = viewport_v / image_height;

// Calculate the location of the upper left pixel.
auto viewport_upper_left = camera_center - vec3(0, 0, focal_length) - viewport_u/2 - viewport_v/2;
auto pixel00_loc = viewport_upper_left + 0.5 * (pixel_delta_u + pixel_delta_v);
```
-/
public def mk'
  [Add α] [Sub α] [Neg α] [Mul α] [Div α] [Zero α] [One α]
  [NatToα : Coe Nat α]
  (focalLength : α) (height : α) (camera : Camera α) (resolution : Resolution)
  : Viewport α :=
  let two : α := 1 + 1

  let width := height * resolution.aspectRatio

  let x := camera.right * width
  let y := camera.up * -height
  let z := camera.forward * focalLength

  let center := camera.center + z
  let topLeft := center - ((x + y) / two)

  let pixelΔX := x / (resolution.columnCount : α)
  let pixelΔY := y / (resolution.rowCount : α)
  let pixelTopLeft := topLeft + ((pixelΔX + pixelΔY) / two)

  ⟨pixelTopLeft, pixelΔX, pixelΔY⟩

end Viewport

end viewport
