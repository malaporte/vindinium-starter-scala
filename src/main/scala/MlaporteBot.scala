package bot

import java.awt.Desktop
import java.net.URI

import bot.Dir.Dir

class MlaporteBot extends Bot {
  private var opened = false

  def move(input: Input): Dir = {
    if (!opened) {
      Desktop.getDesktop.browse(new URI(s"http://vindinium.org/${input.game.id}"))
      opened = true
      return Dir.Stay
    }

    new MoveComputer(input).compute()
  }
}
