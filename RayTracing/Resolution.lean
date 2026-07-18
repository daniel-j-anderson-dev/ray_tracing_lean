namespace resolution

public structure Resolution where
  rowCount : Nat
  columnCount : Nat
  deriving Repr

namespace Resolution

open Resolution

public abbrev Float.toNat (f : Float) : Nat :=
  f.toUInt64.toNat

public abbrev Rat.absFloat (r : Rat) : Float :=
  r.abs.num.toNat.toFloat / r.den.toFloat

public def fromColumnCountIdealAspectRatio
  (columnCount : Nat) (idealAspectRatio : Rat)
  : Resolution :=
  let rowCount := Float.toNat (columnCount.toFloat / idealAspectRatio.absFloat)
  let rowCount := if rowCount < 1 then 1 else rowCount
  ⟨rowCount, columnCount⟩

public def aspectRatio [NatToα : Coe Nat α] [Div α] (self : Resolution) : α :=
  (self.columnCount : α) / (self.rowCount : α)

public def pixelCount (self : Resolution) : Nat :=
  self.rowCount * self.columnCount

public def pixelRowIndex (self : Resolution) (pixelIndex : Nat) : Nat :=
  pixelIndex / self.columnCount

public def pixelColumnIndex (self : Resolution) (pixelIndex : Nat) : Nat :=
  pixelIndex % self.columnCount

public def deserializePixelIndex (self : Resolution) (serialPixelIndex: Nat) : Nat × Nat :=
  (self.pixelRowIndex serialPixelIndex, self.pixelColumnIndex serialPixelIndex)

public def serializeIndex (self : Resolution) (deserializedIndex : Nat × Nat) : Nat :=
  let (rowIndex, columnIndex) := deserializedIndex
  rowIndex * self.columnCount + columnIndex

public def pixelIndexes (self : Resolution) : Array (Nat × Nat) :=
  (Array.range self.rowCount).flatMap λ rowIndex =>
    (Array.range self.columnCount).map λ columnIndex =>
      (rowIndex, columnIndex)

end Resolution

end resolution
