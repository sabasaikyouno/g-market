import cats.effect.IO
import com.microsoft.playwright.{Browser, BrowserType, Playwright}
import io.circe.Json
import io.circe.parser.parse
import org.apache.commons.lang.StringUtils

import io.circe._
import io.circe.parser._

import scala.concurrent.duration.DurationInt
import scala.jdk.CollectionConverters._

case class GameUrl(url: String, gameTitle: String)

object GameUrls {

  def getPcGameUrls(playwright: Playwright) = ???

  private def getGameTradeUrls(playwright: Playwright) = for {
    browser <- IO(playwright.chromium().launch())
    page <- IO(browser.newContext(new Browser.NewContextOptions().setUserAgent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:109.0) Gecko/20100101 Firefox/109.0")).newPage())
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

  private def getGameClubUrls(playwright: Playwright) = for {
    browser <- IO(playwright.chromium().launch())
    page <- IO(browser.newContext(new Browser.NewContextOptions().setUserAgent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:109.0) Gecko/20100101 Firefox/109.0")).newPage())
    _ <- IO(page.navigate("https://gameclub.jp/"))
    _ <- IO(page.click(".word-search.btn-modal.btn-search-title"))
    _ <- IO.sleep(5.seconds)
    eleList <- IO(page.locator(".syllabary-list div").all().asScala.toList)
    gameUrls = eleList.map { ele =>
      val json = parseGameClubJson(ele.getAttribute("data-item"))
      GameUrl(s"https://gameclub.jp${json._1}", json._2)
    }
  } yield gameUrls

  private def getRmtClubUrls(playwright: Playwright) = for {
    browser <- IO(playwright.chromium().launch(new BrowserType.LaunchOptions().setHeadless(false)))
    page <- IO(browser.newContext(new Browser.NewContextOptions().setUserAgent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:109.0) Gecko/20100101 Firefox/109.0")).newPage())
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

  private def similarity(s1: String, s2: String) = {
    val maxLength = s1.length.max(s2.length)

    1.0 - (StringUtils.getLevenshteinDistance(s1, s2).toDouble / maxLength)
  }
}
