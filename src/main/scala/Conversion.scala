import models.GMarketDT

trait Conversion[A] {
  def toGMarket(a: A): GMarketDT
}
