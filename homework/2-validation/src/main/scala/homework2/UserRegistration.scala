package homework2

case class RegistrationForm(
    name: String,
    email: String,
    password: String,
    passwordConfirmation: String,
    birthYear: String,
    birthMonth: String,
    birthDay: String,
    postalCode: String
)

sealed trait RegistrationFormError

case object NameIsEmpty extends RegistrationFormError

case class InvalidEmail(email: String) extends RegistrationFormError

case object PasswordTooShort                     extends RegistrationFormError
case object PasswordRequiresGreaterSymbolVariety extends RegistrationFormError
case object PasswordsDoNotMatch                  extends RegistrationFormError

case class InvalidBirthdayDate(dateErrors: Chain[DateError])
    extends RegistrationFormError
case class BirthdayDateIsInTheFuture(date: Date) extends RegistrationFormError

case class InvalidPostalCode(code: String) extends RegistrationFormError

sealed trait DateError
case class YearIsNotAnInteger(year: String)   extends DateError
case class MonthIsNotAnInteger(month: String) extends DateError
case class DayIsNotAnInteger(day: String)     extends DateError
case class MonthOutOfRange(month: Int)        extends DateError
case class DayOutOfRange(day: Int)            extends DateError
case class InvalidDate(date: Date)            extends DateError

case class Email(user: String, domain: String)

case class User(
    name: String,
    email: Email,
    passwordHash: String,
    birthday: Date,
    postalCode: Option[String]
)

object UserRegistration {
  def registerUser(
      userCountryPostalCodeVerifier: String => Boolean,
      today: Date
  )(form: RegistrationForm): Validated[RegistrationFormError, User] =
    (
      validateName(form.name),
      validateEmail(form.email),
      validatePassword(form.password, form.passwordConfirmation),
      validateDate(form.birthYear, form.birthMonth, form.birthDay)
        .fold(
          errors => Invalid(InvalidBirthdayDate(errors)),
          validateBirtday(_, today)
        ),
      validatePostalCode(form.postalCode, userCountryPostalCodeVerifier)
    ).zipMap(User.apply)

  def validateDate(
      year: String,
      month: String,
      day: String
  ): Validated[DateError, Date] = {
    import Validated._

    def toInteger(string: String): Option[Int] =
      try {
        Some(string.toInt)
      } catch {
        case _: NumberFormatException => None
      }

    def validateInteger(
        string: String,
        error: => DateError
    ): Validated[DateError, Int] =
      toInteger(string).toValidated(error)

    def boundCheck(
        number: Int,
        min: Int,
        max: Int,
        error: => DateError
    ): Validated[DateError, Int] =
      if ((min to max).contains(number)) Valid(number) else Invalid(error)

    def validityCheck(date: Date): Validated[DateError, Date] =
      Date
        .applyOption(date.year, date.month, date.day)
        .toValidated(InvalidDate(date))

    (
      validateInteger(year, YearIsNotAnInteger(year)),
      validateInteger(month, MonthIsNotAnInteger(month))
        .flatMap(month => boundCheck(month, 1, 12, MonthOutOfRange(month))),
      validateInteger(day, DayIsNotAnInteger(day))
        .flatMap(day => boundCheck(day, 1, 31, DayOutOfRange(day)))
    ).zipMap(Date.apply).flatMap(validityCheck)
  }

  private def validateName(
      name: String
  ): Validated[RegistrationFormError, String] =
    if (name.isEmpty) Invalid(NameIsEmpty) else Valid(name)

  private def validateEmail(
      email: String
  ): Validated[RegistrationFormError, Email] =
    email.split('@') match {
      case Array(user, domain) if !user.isEmpty && !domain.isEmpty =>
        Valid(Email(user, domain))
      case _ => Invalid(InvalidEmail(email))
    }

  private def validatePassword(
      password: String,
      passwordConfirmation: String
  ): Validated[RegistrationFormError, String] = {
    val numbers = '0' to '9'
    val letters = ('A' to 'Z') ++ ('a' to 'z')

    def highPasswordVariety(password: String): Boolean =
      password.exists(numbers.contains(_)) &&
        password.exists(letters.contains(_)) &&
        !password.forall(c => letters.contains(c) || numbers.contains(c))

    (
      if (password.length >= 8) Valid(password) else Invalid(PasswordTooShort),
      if (highPasswordVariety(password)) Valid(password)
      else Invalid(PasswordRequiresGreaterSymbolVariety),
      if (password == passwordConfirmation) Valid(password)
      else Invalid(PasswordsDoNotMatch)
    ).zipMap((password, _, _) => PasswordUtils.hash(password))
  }

  private def validateBirtday(
      birthday: Date,
      today: Date
  ): Validated[RegistrationFormError, Date] = {
    import scala.math.Ordered.orderingToOrdered
    def compareDates(a: Date, b: Date): Int =
      (a.year, a.month, a.day) compare (b.year, b.month, b.day)

    if (compareDates(birthday, today) <= 0)
      Valid(birthday)
    else
      Invalid(BirthdayDateIsInTheFuture(birthday))
  }

  private def validatePostalCode(
      postalCode: String,
      userCountryPostalCodeVerifier: String => Boolean
  ): Validated[RegistrationFormError, Option[String]] =
    if (postalCode.isEmpty)
      Valid(None)
    else if (userCountryPostalCodeVerifier(postalCode))
      Valid(Some(postalCode))
    else
      Invalid(InvalidPostalCode(postalCode))
}
