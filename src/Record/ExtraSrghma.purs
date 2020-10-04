module Record.ExtraSrghma
  ( module Export )
  where

import Record.ExtraSrghma.MapIndex (class MapIndex, mapIndex, mapIndexBuilder) as Export
import Record.ExtraSrghma.MapValuesWithIndex (class MapValuesWithIndex, mapValuesWithIndex, mapValuesWithIndexBuilder) as Export
import Record.ExtraSrghma.ParSequenceRecord (class ParSequenceRecord, parSequenceRecord, parSequenceRecordImpl) as Export
