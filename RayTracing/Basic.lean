namespace sqrt

public class Sqrt α where
  sqrt : α → α

public instance Float32.Sqrt : Sqrt Float32 where
  sqrt := Float32.sqrt

public instance Float.Sqrt : Sqrt Float where
  sqrt := Float.sqrt

public def sqrt [s : Sqrt α] (x : α) : α := s.sqrt x

end sqrt

namespace clamp

public def clamp
  [Max α] [Min α]
  (value : α) (lower : α) (upper : α)
  : α :=
  max lower (min value upper)

end clamp
