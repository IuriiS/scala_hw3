package homeworks.futures

import homeworks.HomeworksUtils.TaskSyntax

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

object task_futures_sequence {

  /**
   * В данном задании Вам предлагается реализовать функцию fullSequence,
   * похожую на Future.sequence, но в отличии от нее,
   * возвращающую все успешные и не успешные результаты.
   * Возвращаемое тип функции - кортеж из двух списков,
   * в левом хранятся результаты успешных выполнений,
   * в правово результаты неуспешных выполнений.
   * Не допускается использование методов объекта Await и мутабельных переменных var
   */
  /**
   * @param futures список асинхронных задач
   * @return асинхронную задачу с кортежом из двух списков
   */
  def fullSequence2[A](futures: List[Future[A]])
                     (implicit ex: ExecutionContext): Future[(List[A], List[Throwable])] = {
    @tailrec
    def loop(list: List[Future[A]], accSuccess: List[A], accFailure: List[Throwable]): (List[A], List[Throwable]) = {
      list match {
        case head :: tail => {
          while (!head.isCompleted) {
            Thread.sleep(50)
          }

          head.value match {
            case Some(res) => res match {
              case Success(value) => loop(tail, accSuccess.appended(value), accFailure)
              case Failure(exception) => loop(tail, accSuccess, accFailure.appended(exception))
            }
            //TODO: this case should not happen, but better think about what to do here
            case None => loop(tail, accSuccess, accFailure)
          }
        }
        case Nil => (accSuccess, accFailure)
      }
    }

    Future {
      loop(futures, List(), List())
    }
  }

  def fullSequence[A](futures: List[Future[A]])
                     (implicit ex: ExecutionContext): Future[(List[A], List[Throwable])] = {

    def loop(lst : List[Future[A]], accSuccess : List[A], accFailure : List[Throwable]) : Future[(List[A], List[Throwable])] = {
        lst match {
          case Nil => Future { (accSuccess, accFailure) }
          case head :: tail => head.transformWith {
            case Success(res) => loop(tail, accSuccess.appended(res), accFailure)
            case Failure(exception) => loop(tail, accSuccess, accFailure.appended(exception))
          }
        }
    }

    loop(futures, List(), List())
  }
}
