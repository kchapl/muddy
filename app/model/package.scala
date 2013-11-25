import scala.math.BigDecimal.RoundingMode

package object model {
  def round(num: Double) = BigDecimal(num).setScale(2, RoundingMode.HALF_UP).toDouble
}
