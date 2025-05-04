module Record.ExtraSrghma
  ( module Export
  ) where

-- find ./src/Record/ExtraSrghma -type f -name "*.purs" | while read file; do
--   filename=$(basename "$file" .purs)
--   echo "import Record.ExtraSrghma.$filename as Export"
-- done

import Record.ExtraSrghma.AppendRecord (class AppendSubrecordImpl, appendRecord, appendSubrecordImpl) as Export
import Record.ExtraSrghma.MapRecord (class MapRecord, mapRecord, mapRecordBuilder) as Export
import Record.ExtraSrghma.SList (class SListToRowList, type (:::), SCons, SList, SNil) as Export
import Record.ExtraSrghma.SequenceRecord (class SequenceRecord, sequenceRecord, sequenceRecordImpl) as Export
import Record.ExtraSrghma.MapValuesWithIndex (class MapValuesWithIndex, mapValuesWithIndex, mapValuesWithIndexBuilder) as Export
import Record.ExtraSrghma.ParSequenceRecord (class ParSequenceRecord, parSequenceRecord, parSequenceRecordImpl) as Export
import Record.ExtraSrghma.FoldrValuesLazy (class FoldrValuesLazy, class FoldrValuesLazy1, foldMapValuesLazyR, foldMapValuesLazyR1, foldrValuesLazy, foldrValuesLazy1, foldrValuesLazyImpl, foldrValuesLazyImpl1) as Export
import Record.ExtraSrghma.CompareRecord (class OrdRecord, compareRecord, compareRecordImpl) as Export
import Record.ExtraSrghma.MapIndex (class MapIndex, mapIndex, mapIndexBuilder) as Export
import Record.ExtraSrghma.TraverseRecord (class TraverseRecord, traverseRecord, traverseRecordImpl) as Export
import Record.ExtraSrghma.FoldrValues (class FoldrValues, class FoldrValues1, foldMapValuesR, foldMapValuesR1, foldrValues, foldrValues1, foldrValuesImpl, foldrValuesImpl1) as Export
import Record.ExtraSrghma.ZipRecord (class ZipRecord, zipRecord, zipRecordImpl) as Export
import Record.ExtraSrghma.FoldlValues (class FoldlValues, class FoldlValues1, foldMapValuesL, foldMapValuesL1, foldlValues, foldlValues1, foldlValuesImpl, foldlValuesImpl1) as Export
import Record.ExtraSrghma.ValuesToUnfoldableLazy (LazyTupleList(..), valuesToUnfoldableLazy) as Export
import Record.ExtraSrghma.FoldlValuesWithIndex (class FoldlValuesWithIndex, class FoldlValuesWithIndex1, foldMapValuesWithIndexL, foldMapValuesWithIndexL1, foldlValuesWithIndex, foldlValuesWithIndex1, foldlValuesWithIndexImpl, foldlValuesWithIndexImpl1) as Export
import Record.ExtraSrghma.Keys (class RecordKeys, class RecordKeys1, class RowListKeys, class RowNonEmptyListKeys, pick, pickFn, recordKeys, recordKeys', recordKeys1, recordKeys1', rowKeys, rowKeys', rowKeys1, rowKeys1', rowListKeys, rowNonEmptyListKeys, slistKeys, slistKeys', slistKeys1, slistKeys1') as Export
import Record.ExtraSrghma.FoldrValuesWithIndex (class FoldrValuesWithIndex, class FoldrValuesWithIndex1, foldMapValuesWithIndexR, foldMapValuesWithIndexR1, foldrValuesWithIndex, foldrValuesWithIndex1, foldrValuesWithIndexImpl, foldrValuesWithIndexImpl1) as Export
