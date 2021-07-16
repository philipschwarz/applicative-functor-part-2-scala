// Slides 36-38 https://www.slideshare.net/pjschwarz/applicative-functor-part-2#36
object _2_Username_and_password_validation_using_Either_Applicative extends App {

  case class Username(username: String)
  case class Password(password:String)
  case class Error(error:String)
  case class User(username: Username, password: Password)

  def checkUsernameLength(username: String): Either[Error, Username] =
    username.length > 15 match {
      case true  => Left(Error("Your username cannot be longer than 15 characters."))
      case false => Right(Username(username))
    }

  def checkPasswordLength(password: String): Either[Error, Password] =
    password.length > 20 match {
      case true  => Left(Error("Your password cannot be longer than 20 characters."))
      case false => Right(Password(password))
    }

  def requireAlphaNum(password: String): Either[Error, String] =
    password.forall(_.isLetterOrDigit) match {
      case false  => Left(Error("Cannot contain white space or special characters."))
      case true   => Right(password)
    }

  def cleanWhitespace(password:String): Either[Error, String] =
    password.dropWhile(_.isWhitespace) match {
      case pwd if pwd.isEmpty => Left(Error("Cannot be empty."))
      case pwd                => Right(pwd)
    }

  def validateUsername(username: Username): Either[Error,Username] = username match {
    case Username(username) =>
      cleanWhitespace(username)
        .flatMap(requireAlphaNum)
        .flatMap(checkUsernameLength)
  }

  def validatePassword(password: Password): Either[Error,Password] = password match {
    case Password(pwd) =>
      cleanWhitespace(pwd)
        .flatMap(requireAlphaNum)
        .flatMap(checkPasswordLength)
  }

  trait Functor[F[_]] {
    def map[A,B](fa: F[A])(f: A => B): F[B]
  }

  trait Applicative[F[_]] extends Functor[F] {
    def <*>[A,B](fab: F[A => B],fa: F[A]): F[B]
    def unit[A](a: => A): F[A]
    def map[A,B](fa: F[A])(f: A => B): F[B] =
      <*>(unit(f),fa)
  }

  type Validation[A] = Either[Error, A]

  val eitherApplicative = new Applicative[Validation] {

    def <*>[A,B](fab: Validation[A => B],fa: Validation[A]): Validation[B] =
      (fab, fa) match {
        case (Right(ab), Right(a)) => Right(ab(a))
        case (Left(e), _) => Left(e)
        case (_, Left(e)) => Left(e)
      }

    def unit[A](a: => A): Validation[A] = Right(a)

  }

  import eitherApplicative._

  def makeUser(name: Username, password: Password): Either[Error, User] =
    <*>( map(validateUsername(name))(User.curried),
         validatePassword(password) )

  import cats.effect.IO

  def getLine: IO[String] = IO { scala.io.StdIn.readLine }
  def print(s: String): IO[Unit] = IO { scala.Predef.print(s) }

  val main: IO[Unit] =
    for {
      _   <- print("Please enter a username.\n")
      usr <- getLine map Username
      _   <- print("Please enter a password.\n")
      pwd <- getLine map Password
      _   <- print(makeUser(usr,pwd).toString)
    } yield ()

  main.unsafeRunSync

}
