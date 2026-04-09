package net.chwthewke.stnu
package spa
package prod

case class SplitMergePreview(
    originalBoostedMachineCount: Option[Int],
    result: List[Double],
    overflow: Boolean // display warning if true (merge), success if false (split)
)
