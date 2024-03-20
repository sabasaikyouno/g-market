import cats._
import cats.data._
import cats.syntax.all._
import cats.effect.IO
import com.microsoft.playwright.{Browser, Playwright}
import models.RmtClubDT

import scala.jdk.CollectionConverters._

object GetRmtClubData {
  def getRmtClubData(playwright: Playwright, url: String) = {
    val itemELeList = for {
      itemEleList <- getCategoryUrls(url).traverse(url => getItemEleList(playwright, url))
    } yield itemEleList.flatten

    itemELeList.flatMap { list =>
      list.traverse { ele =>
        for {
          title <- IO(ele.locator(".item-texts .title a").innerHTML())
          imgSrc <- IO(ele.locator(".item-thumb img").getAttribute("src"))
          gameTitle <- IO(ele.locator(".item-texts .game-title a").nth(1).innerHTML())
          detail <- IO(ele.locator(".text").innerHTML())
          price <- IO(parsePrice(ele.locator(".flex .price span").innerHTML()))
          url <- IO("https://rmt.club" + ele.locator(".item-texts .title a").getAttribute("href"))
          category <- IO(ele.page().url().last.toString)
        } yield RmtClubDT(title, imgSrc, gameTitle, detail, price, url, category)
      }
    }
  }

  private def getItemEleList(playwright: Playwright, url: String) = for {
    browser <- IO(playwright.chromium().launch())
    page <- IO(browser.newContext(new Browser.NewContextOptions().setUserAgent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:109.0) Gecko/20100101 Firefox/109.0")).newPage())
    _ <- IO(page.navigate(url))
    itemsEle <- IO(page.locator(".post-list-row .item").all().asScala.toList)
  } yield itemsEle

  private def getCategoryUrls(url: String) = {
    val dealAccountId = List(1,2,4)

    dealAccountId.map(i => s"$url&deal_type_id=2&sort=selling&deal_account_id=$i")
  }

  private def parsePrice(str: String) = if (str == "査定中") -1 else str.replace(",", "").toInt
}
