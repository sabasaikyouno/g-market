import cats._, cats.data._, cats.syntax.all._
import cats.effect.IO
import com.microsoft.playwright.{Browser, Playwright}
import models.GameTradeDT
import io.circe.parser.decode
import scala.jdk.CollectionConverters._

object GetGameTradeData {
  def getGameTradeData(playwright: Playwright, url: String) = {
    val itemEleList = for {
      categoryUrls <- getCategoryUrlList(playwright, url)
      itemLists <- categoryUrls.traverse(categoryUrl => getItemEleList(playwright, categoryUrl))
    } yield itemLists.flatten

    itemEleList.flatMap { list =>
      list.traverse { ele =>
        for {
          title <- IO(ele.locator(".detail h3").innerHTML())
          imgSrc <- IO(ele.locator(".game-image img").getAttribute("src"))
          gameTitle <- IO(parseGameTitle(ele.locator(".game-image img").getAttribute("alt")))
          detail <- IO(ele.locator(".detail .description p").innerHTML())
          price <- IO(parsePrice(ele.locator(".detail .price .current_price p").innerHTML()))
          url <- IO("https://gametrade.jp" + ele.locator(".exhibit-link").getAttribute("href"))
        } yield GameTradeDT(title,imgSrc, gameTitle, detail, price, url)
      }
    }
  }

  private def getItemEleList(playwright: Playwright, url: String) = for {
    browser <- IO(playwright.chromium().launch())
    page <- IO(browser.newContext(new Browser.NewContextOptions().setUserAgent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:109.0) Gecko/20100101 Firefox/109.0")).newPage())
    _ <- IO(page.navigate("https://gametrade.jp" + url))
    itemsEle <- IO(page.locator("//ul[@class='exhibits clearfix']/li[@data-index]").all().asScala.toList)
  } yield itemsEle

  private def getCategoryUrlList(playwright: Playwright, url: String) = for {
    browser <- IO(playwright.chromium().launch())
    page <- IO(browser.newContext(new Browser.NewContextOptions().setUserAgent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:109.0) Gecko/20100101 Firefox/109.0")).newPage())
    _ <- IO(page.navigate(url))
    categoryUrlList <- IO(page.locator(".tabs a").all().asScala.map(_.getAttribute("href")).toList)
  } yield categoryUrlList

  private def parseGameTitle(alt: String) = {
    val p = """.*\|(.*)""".r

    p.findFirstMatchIn(alt).map(_.group(1)).get
  }

  private def parsePrice(price: String) = price.replace("¥", "").replace(",", "").toInt
}
