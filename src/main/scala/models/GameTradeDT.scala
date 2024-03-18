package models

import io.circe._
import io.circe.generic.semiauto._

case class GameTradeDT (
  item_id: Long,
  item_name: String,
  index: Int,
  item_brand: String,
  item_category: Int,
  item_list_id: Int,
  item_list_name: String,
  price: Int,
  quantity: Int,
  discount: Option[Int]
)

object GameTradeDT {
  implicit val decoder: Decoder[GameTradeDT] = deriveDecoder[GameTradeDT]
  implicit val encoder: Encoder[GameTradeDT] = deriveEncoder[GameTradeDT]
}
