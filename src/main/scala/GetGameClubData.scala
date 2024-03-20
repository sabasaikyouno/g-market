import cats._, cats.data._, cats.syntax.all._
import cats.effect.IO
import com.microsoft.playwright.{Browser, BrowserType, Playwright}
import models.GameClubDT

import scala.jdk.CollectionConverters._

object GetGameClubData {
  def getGameClubData(playwright: Playwright, url: String) = {
    val itemEleList = getItemEleList(playwright, url)

    itemEleList.flatMap { list =>
      list.traverse { ele =>
        for {
          title <- IO(ele.locator(".title h3 a").innerHTML())
          imgSrc <- IO(ele.locator(".item-thumb").getAttribute("src"))
          gameTitle <- IO(ele.locator(".game-title a span").innerHTML())
          detail <- IO(ele.locator(".detail").innerHTML())
          price <- IO(parsePrice(ele.locator(".price").innerHTML()))
          url <- IO(ele.locator(".title h3 a").getAttribute("href"))
          category <- IO(ele.locator("//div[@class='item-row-top']/span[2]").innerHTML())
        } yield GameClubDT(title,imgSrc, gameTitle, detail, price, url, category)
      }
    }
  }

  private def getItemEleList(playwright: Playwright, url: String) = for {
    browser <- IO(playwright.chromium().launch())
    page <- IO(browser.newContext(new Browser.NewContextOptions().setUserAgent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:109.0) Gecko/20100101 Firefox/109.0")).newPage())
    _ <- IO(page.navigate(url))
    itemsEle <- IO(page.locator(".item-list .item-row").all().asScala.toList)
  } yield itemsEle

  private def parsePrice(price: String) = price.replace("¥", "").replace(",", "").toInt
}
