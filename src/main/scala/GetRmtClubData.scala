import cats.effect.IO
import cats.syntax.all._
import com.microsoft.playwright.Page
import models.RmtClubDT

import scala.jdk.CollectionConverters._

object GetRmtClubData {
  def getRmtClubData(url: String, page: Page) = {
    val itemELeList = for {
      itemEleList <- getCategoryUrls(url).traverse(url => getItemEleList(url, page))
    } yield itemEleList.flatten

    itemELeList.flatMap { list =>
      list.filter(_.isVisible).traverse { ele =>
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

  private def getItemEleList(url: String, page: Page) = for {
    _ <- IO(page.navigate(url))
    itemEle <- IO(page.locator(".post-list-row .item").all().asScala.toList)
  } yield itemEle

  private def getCategoryUrls(url: String) = {
    val dealAccountId = List(1,2,4)

    dealAccountId.map(i => s"$url&deal_type_id=2&sort=selling&deal_account_id=$i")
  }

  private def parsePrice(str: String) = if (str == "査定中") -1 else str.replace(",", "").toInt
}
