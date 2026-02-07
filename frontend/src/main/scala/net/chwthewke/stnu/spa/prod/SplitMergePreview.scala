package net.chwthewke.stnu
package spa
package prod

case class SplitMergePreview(
    result: List[Double],
    overflow: Boolean // display warning if true (merge), success if false (split)
)
