import cats._
import cats.data._
import cats.syntax.all._
import cats.effect.IO
import com.microsoft.playwright.{Browser, BrowserType, Page, Playwright}
import models.GameClubDT

import scala.jdk.CollectionConverters._

object GetGameClubData {
  def getGameClubData(url: String, page: Page) = {
    val itemEleList = getItemEleList(url, page)
    itemEleList.flatMap { list =>
      list.filter(_.locator(".game-title").isVisible()).traverse { ele =>
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

  private def getItemEleList(url: String, page: Page) = for {
    _ <- IO(page.navigate(url))
    itemsEle <- IO(page.locator(".item-list .item-row").all().asScala.toList)
  } yield itemsEle

  private def parsePrice(price: String) = price.replace("¥", "").replace(",", "").toInt
}
