import cats.effect.IO
import com.microsoft.playwright.Playwright
import models.GameTradeDT
import io.circe.parser.decode

object GetGameTradeData {
  def getGameTradeData(playwright: Playwright, url: String): IO[Array[GameTradeDT]] = for {
    browser <- IO(playwright.chromium().launch())
    page <- IO(browser.newPage())
    _ <- IO(page.navigate(url))
    json <- IO(page.evaluate("() => JSON.stringify(dataLayer[8]['ecommerce']['items'])").toString)
    items <- IO.fromEither(decode[Array[GameTradeDT]](json))
    _ <- IO(browser.close())
  } yield items
}
