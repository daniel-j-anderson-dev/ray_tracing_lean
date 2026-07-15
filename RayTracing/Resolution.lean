namespace resolution

public structure Resolution where
  rowCount : Nat
  columnCount : Nat

namespace Resolution

public abbrev Float.toNat (f : Float) : Nat :=
  f.toUInt64.toNat

public def fromColumnCountIdealAspectRatio (columnCount : Nat) (idealAspectRatio : Float) : Resolution :=
  let rowCount := Float.toNat (columnCount.toFloat / idealAspectRatio)
  ⟨if rowCount < 1 then 1 else rowCount, columnCount⟩

public def aspectRatio [Coe Nat β] [Div β] (self : Resolution) : β :=
  (self.columnCount : β) / (self.rowCount : β)

public def pixelCount (self : Resolution) : Nat :=
  self.rowCount * self.columnCount

public def pixelRowIndex (self : Resolution) (pixelIndex : Nat) : Nat :=
  pixelIndex / self.columnCount

public def pixelColumnIndex (self : Resolution) (pixelIndex : Nat) : Nat :=
  pixelIndex % self.columnCount

public def pixelIndexes (self : Resolution) : Array (Nat × Nat) :=
  (Array.range self.rowCount).flatMap λ rowIndex =>
    (Array.range self.columnCount).map λ columnIndex =>

      (rowIndex, columnIndex)
end Resolution

end resolution
