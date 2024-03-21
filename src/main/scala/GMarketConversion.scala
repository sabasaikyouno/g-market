import models.{GMarketDT, GameClubDT, RmtClubDT}

object GMarketConversion {
  implicit class ConvertOps[A: Conversion](a: A) {
    def toDt = {
      implicitly[Conversion[A]].toGMarket(a)
    }
  }

  implicit val gameClubDT = new Conversion[GameClubDT] {
    def toGMarket(dt: GameClubDT) = GMarketDT(
      dt.title,
      dt.imgSrc,
      dt.gameTitle,
      dt.detail,
      dt.price,
      dt.url,
      dt.category,
      "GameClub"
    )
  }
}

