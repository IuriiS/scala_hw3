import scala.concurrent.Future

import scala.concurrent.ExecutionContext.Implicits.global

val futureList = List(Future(() => {
  Thread.sleep(2000)
  1
}
), Future(() => {
  Thread.sleep(3000)
  2
},
  Future(( ) => {
    Thread.sleep(4000)
    3
  })))

//futureList.map( f => f.onComplete( ))

val resSuccess = List

futureList.sliding(2).foreach((lst) => println(lst))