
// Slide 11 https://www.slideshare.net/pjschwarz/applicative-functor-part-2#11
object _1_Password_validation_using_Either_Monad extends App {

   case class Password(password:String)
   case class Error(error:String)

   def checkPasswordLength(password: String): Either[Error, Password] =
      password.length > 20 match {
         case true  => Left(Error("Your password cannot be longer than 20 characters."))
         case false => Right(Password(password))
      }

   def requireAlphaNum(password: String): Either[Error, String] =
      password.forall(_.isLetterOrDigit) match {
         case false  => Left(Error("Your password cannot contain white space or special characters."))
         case true   => Right(password)
      }

   def cleanWhitespace(password:String): Either[Error, String] =
      password.dropWhile(_.isWhitespace) match {
         case pwd if pwd.isEmpty => Left(Error("Your password cannot be empty."))
         case pwd                => Right(pwd)
   }

   def validatePassword(password: Password): Either[Error,Password] = password match {
      case Password(pwd) =>
         cleanWhitespace(pwd)
           .flatMap(requireAlphaNum)
           .flatMap(checkPasswordLength)
   }

   import cats.effect.IO

   def getLine: IO[String] = IO { scala.io.StdIn.readLine }
   def print(s: String): IO[Unit] = IO { scala.Predef.print(s) }

   val main: IO[Unit] =
      for {
         _   <- print("Please enter a password.\n")
         pwd <- getLine map Password
         _   <- print(validatePassword(pwd).toString)
      } yield ()

   main.unsafeRunSync

}
