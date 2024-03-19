import GetGameClubData.getGameClubData
import GetGameTradeData.getGameTradeData
import cats.effect.{ExitCode, IO, IOApp}
import com.microsoft.playwright._
import io.circe._
import io.circe.parser.decode
import models.GameTradeDT

object Main extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = for {
    playwright <- IO(Playwright.create())
    items <- getGameClubData(playwright, "https://gameclub.jp/lol")
    _ <- IO.println(items)
    _ <- IO(playwright.close())
  } yield ExitCode.Success
}
