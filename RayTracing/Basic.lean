namespace uint8_max

public def UInt8.max : UInt8 := 255

end uint8_max

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

namespace infinity

public class Infinity α where
  infinity : α

public def infinity [Infinity α] : α := Infinity.infinity

public instance Float.Infinity : Infinity Float where
  infinity := 1 / 0

public instance Float32.Infinity : Infinity Float32 where
  infinity := 1 / 0

end infinity
