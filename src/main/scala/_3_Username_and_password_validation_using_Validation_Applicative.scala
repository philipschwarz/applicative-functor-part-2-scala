// Slides 40-42 https://www.slideshare.net/pjschwarz/applicative-functor-part-2#40
object _3_Username_and_password_validation_using_Validation_Applicative extends App {

  trait Semigroup[A] {
    def <>(lhs: A, rhs: A): A
  }

  trait Functor[F[_]] {
    def map[A,B](fa: F[A])(f: A => B): F[B]
  }

  trait Applicative[F[_]] extends Functor[F] {
    def <*>[A,B](fab: F[A => B],fa: F[A]): F[B]
    def *>[A,B](fa: F[A],fb: F[B]): F[B]
    def unit[A](a: => A): F[A]
    def map[A,B](fa: F[A])(f: A => B): F[B] =
      <*>(unit(f),fa)
  }

  sealed trait Validation[+E, +A]
  case class Failure[E](error: E) extends Validation[E, Nothing]
  case class Success[A](a: A) extends Validation[Nothing, A]

  def validationApplicative[E](implicit sg:Semigroup[E]): Applicative[λ[α => Validation[E,α]]] =
    new Applicative[λ[α => Validation[E,α]]] {
      def unit[A](a: => A) = Success(a)
      def <*>[A,B](fab: Validation[E,A => B],fa: Validation[E,A]): Validation[E,B] =
        (fab, fa) match {
          case (Success(ab), Success(a)) => Success(ab(a))
          case (Failure(e1), Failure(e2)) => Failure(sg.<>(e1,e2))
          case (Failure(e), _) => Failure(e)
          case (_, Failure(e)) => Failure(e)
        }
      def *>[A,B](fa: Validation[E,A],fb: Validation[E,B]): Validation[E,B] =
        (fa, fb) match {
          case (Success(a), Success(b)) => Success(b)
          case (Failure(e1), Failure(e2)) => Failure(sg.<>(e1,e2))
          case (Failure(e), _) => Failure(e)
          case (_, Failure(e)) => Failure(e)
        }
    }

  case class Username(username: String)
  case class Password(password:String)
  case class Error(error:List[String])
  case class User(username: Username, password: Password)

  def checkUsernameLength(username: String): Validation[Error, Username] =
    username.length > 15 match {
      case true  => Failure(Error(List("Your username cannot be longer than 15 characters.")))
      case false => Success(Username(username))
    }

  def checkPasswordLength(password: String): Validation[Error, Password] =
    password.length > 20 match {
      case true  => Failure(Error(List("Your password cannot be longer than 20 characters.")))
      case false => Success(Password(password))
    }

  def requireAlphaNum(password: String): Validation[Error, String] =
    password.forall(_.isLetterOrDigit) match {
      case false  => Failure(Error(List("Cannot contain white space or special characters.")))
      case true   => Success(password)
    }

  def cleanWhitespace(password:String): Validation[Error, String] =
    password.dropWhile(_.isWhitespace) match {
      case pwd if pwd.isEmpty => Failure(Error(List("Cannot be empty.")))
      case pwd                => Success(pwd)
    }

  implicit val errorSemigroup: Semigroup[Error] = new Semigroup[Error] {
    def <>(lhs: Error, rhs: Error): Error =
      Error(lhs.error ++ rhs.error)
  }
  val errorValidationApplicative = validationApplicative[Error]
  import errorValidationApplicative._

  def validateUsername(username: Username): Validation[Error, Username] = username match {
    case Username(username) =>
       cleanWhitespace(username) match {
        case Failure(err) => Failure(err)
        case Success(username2) => *>(requireAlphaNum(username2),
                                      checkUsernameLength(username2))
      }
  }

  def validatePassword(password: Password): Validation[Error, Password] = password match {
    case Password(pwd) =>
      cleanWhitespace(pwd) match {
        case Failure(err) => Failure(err)
        case Success(pwd2) => *>(requireAlphaNum(pwd2),
                                 checkPasswordLength(pwd2))
      }
  }

  def makeUser(name: Username, password: Password): Validation[Error, User] =
    <*>( map(validateUsername(name))(User.curried),
             validatePassword(password) )

  import cats.effect.IO

  def getLine: IO[String] = IO { scala.io.StdIn.readLine }
  def print(s: String): IO[Unit] = IO { scala.Predef.print(s) }

  val main =
    for {
      _   <- print("Please enter a username.\n")
      usr <- getLine map Username
      _   <- print("Please enter a password.\n")
      pwd <- getLine map Password
      _   <- print(makeUser(usr,pwd).toString)
    } yield ()

  main.unsafeRunSync

}
