package play

import scala.util.chaining._

object Main extends App {

  Form
    .validateForm(
      username = "Joe",
      password = "Passw0r$1234",
      firstName = "John",
      lastName = "Doe",
      age = 21
    )
    .tap(println)

  Form
    .validateForm(
      username = "Joe%%%",
      password = "password",
      firstName = "John",
      lastName = "Doe",
      age = 5
    )
    .tap(println)
  //   println("hello world")
}
