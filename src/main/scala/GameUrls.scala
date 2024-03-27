import cats.effect.IO
import com.microsoft.playwright.{Browser, BrowserType, Locator, Page, Playwright}
import io.circe.Json
import io.circe.parser.parse
import org.apache.commons.lang.StringUtils

import scala.collection.IterableOnce.iterableOnceExtensionMethods
import scala.concurrent.duration.DurationInt
import scala.jdk.CollectionConverters._
import scala.collection.parallel.CollectionConverters._

case class GameUrl(url: String, gameTitle: String)

object GameUrls {

  def getGameUrls(page: Page) = List(
    "APEX Legends" -> List("https://gametrade.jp/apex-legends/exhibits", "https://gameclub.jp/apex-legends", "https://rmt.club/post_list?title=21977"),
    "フォートナイト(Fortnite)" -> List("https://gametrade.jp/fortnight/exhibits", "https://gameclub.jp/fortnight", "https://rmt.club/post_list?title=9633"),
    "VALORANT(ヴァロラント)" -> List("https://gametrade.jp/valorant/exhibits", "https://gameclub.jp/valorant", "https://rmt.club/post_list?title=33056"),
    "あつまれ どうぶつの森(あつ森)" -> List("https://gametrade.jp/atsumare_doubutunomori/exhibits", "https://gameclub.jp/atsumare-doubutunomori", "https://rmt.club/post_list?title=32391"),
    "ドラクエ10(DQX)" -> List("https://gametrade.jp/dqx/exhibits", "https://gameclub.jp/dqx", "https://rmt.club/post_list?title=357"),
    "ポケモン剣盾(ソードシールド)" -> List("https://gametrade.jp/pokemon-sword-shield/exhibits", "https://gameclub.jp/pokemon-sword-shield", "https://rmt.club/post_list?title=29450"),
    "ポケモンSV(スカーレットバイオレット)" -> List("https://gametrade.jp/pokemon-sv/exhibits", "https://gameclub.jp/pokemon-sv", "https://rmt.club/post_list?title=45356"),
    "FF14" -> List("https://gametrade.jp/ff14/exhibits", "https://gameclub.jp/ff14", "https://rmt.club/post_list?title=435"),
    "モンスト" -> List("https://gametrade.jp/monst/exhibits", "https://gameclub.jp/monst", "https://rmt.club/post_list?title=510"),
    "プロスピA" -> List("https://gametrade.jp/prospi-a/exhibits", "https://gameclub.jp/prospi-a", "https://rmt.club/post_list?title=1168"),
    "FGO" -> List("https://gametrade.jp/fate-go/exhibits", "https://gameclub.jp/fate-go", "https://rmt.club/post_list?title=803"),
    "バウンティラッシュ" -> List("https://gametrade.jp/opbr/exhibits", "https://gameclub.jp/opbr", "https://rmt.club/post_list?title=11157"),
    "原神" -> List("https://gametrade.jp/genshin-impact/exhibits", "https://gameclub.jp/genshin-impact", "https://rmt.club/post_list?title=29069"),
    "ポケモンGO" -> List("https://gametrade.jp/pokemon-go/exhibits", "https://gameclub.jp/pokemon-go", "https://rmt.club/post_list?title=3281"),
    "荒野行動" -> List("https://gametrade.jp/knives-out/exhibits", "https://gameclub.jp/knives-out", "https://rmt.club/post_list?title=9632"),
    "パズドラ(パズル＆ドラゴンズ)" -> List("https://gametrade.jp/pazudora/exhibits", "https://gameclub.jp/pazudora", "https://rmt.club/post_list?title=450")
  )
  def getAllGameUrls(page: Page) = for {
    gameTradeUrls <- getGameTradeUrls(page)
    gameClubUrls <- getGameClubUrls(page)
    rmtClubUrls <- getRmtClubUrls(page)
    gameUrls = gameTradeUrls.map { gameTradeUrl =>
      val gameTitle = gameTradeUrl.gameTitle
      gameTitle -> List(gameTradeUrl.url, findSimilar(gameTitle, gameClubUrls), findSimilar(gameTitle, rmtClubUrls))
    }
  } yield gameUrls

  private def getGameTradeUrls(page: Page) = for {
    _ <- IO(page.navigate("https://gametrade.jp/pc-rmt"))
    pcEleList <- IO(page.locator(".exhibits-box div ul li p a").all().asScala.toList)
    pcGameUrls = pcEleList.map( ele =>
      GameUrl(s"https://gametrade.jp${ele.getAttribute("href")}", ele.innerHTML())
    )
    _ <- IO(page.navigate("https://gametrade.jp/sumaho-rmt"))
    mobileEleList <- IO(page.locator(".exhibits-box div ul li").all().asScala.toList)
    mobileGameUrls = mobileEleList.map( ele =>
      GameUrl(s"https://gametrade.jp${ele.locator("a").first().getAttribute("href")}", ele.locator("p").innerHTML())
    )
  } yield pcGameUrls ::: mobileGameUrls

  private def getGameClubUrls(page: Page) = for {
    _ <- IO(page.navigate("https://gameclub.jp/"))
    _ <- IO(page.navigate("https://gameclub.jp/"))
    _ <- IO(page.click(".word-search.btn-modal.btn-search-title"))
    _ <- IO.sleep(5.seconds)
    eleList <- IO(page.locator(".syllabary-list div").all().asScala.toList)
    gameUrls = eleList.map { ele =>
      val json = parseGameClubJson(ele.getAttribute("data-item"))
      GameUrl(s"https://gameclub.jp/${json._1}", json._2)
    }
  } yield gameUrls

  private def getRmtClubUrls(page: Page) = for {
    _ <- IO(page.navigate("https://rmt.club/"))
    _ <- IO(page.navigate("https://rmt.club/"))
    _ <- IO(page.click("#search_box"))
    _ <- IO.sleep(5.seconds)
    eleList <- IO(page.locator(".syllabary-list p span").all().asScala.toList)
    gameUrls = eleList.map { ele =>
      GameUrl(s"https://rmt.club/post_list?title=${ele.getAttribute("data-id")}", ele.innerHTML())
    }
  } yield gameUrls

  private def parseGameClubJson(jsonStr: String) = {
    val cursor = parse(jsonStr).getOrElse(Json.Null).hcursor
    (
      cursor.downField("slug").as[String].getOrElse(""),
      cursor.downField("name").as[String].getOrElse("")
    )
  }

  private def findSimilar(gameTitle: String, gameUrls: List[GameUrl]) = {
    gameUrls.map(gameUrl => (similarity(gameTitle, gameUrl.gameTitle), gameUrl.url)).maxBy(_._1)._2
  }

  private def similarity(s1: String, s2: String) = {
    val maxLength = s1.length.max(s2.length)

    1.0 - (StringUtils.getLevenshteinDistance(s1, s2).toDouble / maxLength)
  }
}
