import models.{GMarketDT, GameClubDT, GameTradeDT, RmtClubDT}

object GMarketConversion {
  implicit class ConvertOps[A: Conversion](a: A) {
    def toDt = {
      implicitly[Conversion[A]].toGMarket(a)
    }
  }

  implicit val rmtClubDT = new Conversion[RmtClubDT] {
    def toGMarket(dt: RmtClubDT) = GMarketDT(
      dt.title,
      dt.imgSrc,
      dt.gameTitle,
      dt.detail,
      dt.price,
      dt.url,
      toGMarketCategory(dt.category),
      "RmtClub"
    )

    def toGMarketCategory: String => String = {
      case "1" => "アカウント"
      case "2" => "アイテム"
      case "4" => "代行"
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
      toGMarketCategory(dt.category),
      "GameClub"
    )

    def toGMarketCategory: String => String = {
      case "引退垢" => "アカウント"
      case "アイテム・通貨" => "アイテム"
      case "代行" => "代行"
      case "注目アカウント" => "その他"
    }
  }

  implicit val gameTradeDT = new Conversion[GameTradeDT] {
    def toGMarket(dt: GameTradeDT) = GMarketDT(
      dt.title,
      dt.imgSrc,
      dt.gameTitle,
      dt.detail,
      dt.price,
      dt.url,
      toGMarketCategory(dt.category),
      "GameTrade"
    )
    def toGMarketCategory: String => String = {
      case "exhibits" => "アカウント"
      case "items" => "アイテム"
      case "agencies" => "代行"
    }

  }
}

