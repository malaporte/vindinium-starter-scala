package bot

object Main {

  // val bot: Bot = new RandomBot
  val bot: Bot = new SlowBot
  // val bot: Bot = new LeftyBot
  // val bot: Bot = new ThirstyBot

  def main(args: Array[String]) = makeServer match {
    case Left(error) ⇒ println(error)
    case Right(server) ⇒ args match {
      case Array() ⇒
        training(server, _.training(100))
      case Array("arena") ⇒
        arena(server, Int.MaxValue)
      case Array("arena", games) ⇒
        arena(server, int(games))
      case Array("training", turns) ⇒
        training(server, _.training(int(turns)))
      case Array("training", turns, map) ⇒
        training(server, _.training(int(turns), Some(map)))
      case a ⇒ println("Invalid arguments: " + a.mkString(" "))
    }
  }

  def arena(server: Server, games: Int) {
    @annotation.tailrec
    def oneGame(it: Int) {
      println(s"[$it/$games] Waiting for pairing...")
      val input = server.arena
      println(s"[$it/$games] Start arena game ${input.viewUrl}")
      try {
        step(server, input)
      }
      catch {
        case e: scalaj.http.HttpException ⇒ println(s"[${e.code}] ${e.body}")
      }
      println(s"\n[$it/$games] Finished arena game ${input.viewUrl}")
      if (it < games) oneGame(it + 1)
    }
    oneGame(1)
  }

  def training(server: Server, boot: Server ⇒ Input) {
    val input = boot(server)
    println("Training game " + input.viewUrl)
    step(server, input)
  }

  @annotation.tailrec
  def step(server: Server, input: Input) {
    if (!input.game.finished) {
      print(".")
      step(server, server.move(input.playUrl, bot move input))
    }
  }

  def makeServer = (
    Option(System.getProperty("server")) getOrElse "http://vindinium.jousse.org",
    System.getProperty("key")
  ) match {
      case (_, null)  ⇒ Left("Specify the user key with -Dkey=mySecretKey")
      case (url, key) ⇒ Right(new Server(url + "/api", key))
    }

  def int(str: String) = java.lang.Integer.parseInt(str)
}
