public structure Resolution where
  rowCount : Nat
  columnCount : Nat

public abbrev Float.toNat (f : Float) : Nat :=
  f.toUInt64.toNat

public def Resolution.fromColumnCountIdealAspectRatio (columnCount : Nat) (idealAspectRatio : Float) : Resolution :=
  let rowCount := (columnCount.toFloat / idealAspectRatio).toNat
  ⟨if rowCount < 1 then 1 else rowCount, columnCount⟩

public def Resolution.aspectRatio (self : Resolution) :=
  self.columnCount.toFloat / self.rowCount.toFloat

public def Resolution.pixelCount (self : Resolution) : Nat :=
  self.rowCount * self.columnCount

public def Resolution.pixelRowIndex (self : Resolution) (pixelIndex : Nat) : Nat :=
  pixelIndex / self.columnCount

public def Resolution.pixelColumnIndex (self : Resolution) (pixelIndex : Nat) : Nat :=
  pixelIndex % self.columnCount

public def Resolution.pixelIndexes (self : Resolution) : Array (Nat × Nat) :=
  (Array.range self.rowCount).flatMap λ rowIndex =>
    (Array.range self.columnCount).map λ columnIndex =>
      (rowIndex, columnIndex)
