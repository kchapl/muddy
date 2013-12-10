import scala.math.BigDecimal.RoundingMode

package object model {
  def round(num: Double, scale: Int = 2) = BigDecimal(num).setScale(scale, RoundingMode.HALF_UP).toDouble
}
