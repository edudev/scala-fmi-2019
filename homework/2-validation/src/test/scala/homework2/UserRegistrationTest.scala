package homework2

import homework2.UserRegistration.registerUser
import org.scalatest.{FlatSpec, Matchers}

class UserRegistrationTest extends FlatSpec with Matchers {
  "An empty form" should "generate errors for the non optional fields" in {
    val emptyForm = RegistrationForm("", "", "", "", "", "", "", "")

    val validation = registerUser(Set.empty, Date(2019, 5, 4))(emptyForm)

    validation.isValid shouldBe false

    val Invalid(errors) = validation
    val errorsSet       = errors.toSet
    val birthdayErrors = errorsSet.collectFirst {
      case InvalidBirthdayDate(dateErrors) => dateErrors.toSet
    }

    errorsSet should have size 5

    errorsSet should contain allOf (
      NameIsEmpty,
      InvalidEmail(""),
      PasswordTooShort,
      PasswordRequiresGreaterSymbolVariety
    )

    birthdayErrors shouldEqual Some(
      Set(
        YearIsNotAnInteger(""),
        MonthIsNotAnInteger(""),
        DayIsNotAnInteger("")
      )
    )
  }

  "A correct form" should "generate no errors and a valid User" in {
    val form = RegistrationForm(
      "ivan",
      "ivan@mail.bg",
      "passw0rd!",
      "passw0rd!",
      "2019",
      "05",
      "16",
      "1000"
    )

    val validation = registerUser(Set("1000"), Date(2019, 5, 16))(form)

    validation.isValid shouldBe true

    val Valid(user) = validation

    user.name shouldBe "ivan"
    user.email shouldBe Email("ivan", "mail.bg")
    user.passwordHash should not be "passw0rd!"
    user.birthday shouldBe Date(2019, 5, 16)
    user.postalCode shouldBe Some("1000")
  }

  "A correct form" should "generate no errors and a valid User with optional postal code" in {
    val form = RegistrationForm(
      "ivan",
      "ivan@mail.bg",
      "passw0rd!",
      "passw0rd!",
      "2009",
      "05",
      "16",
      ""
    )

    val validation = registerUser(Set("1000"), Date(2019, 5, 16))(form)

    validation.isValid shouldBe true

    val Valid(user) = validation

    user.name shouldBe "ivan"
    user.email shouldBe Email("ivan", "mail.bg")
    user.passwordHash should not be "passw0rd!"
    user.birthday shouldBe Date(2009, 5, 16)
    user.postalCode shouldBe None
  }

  "A valid form" should "not have an empty name" in {
    val form = RegistrationForm(
      "",
      "ivan@mail.bg",
      "passw0rd!",
      "passw0rd!",
      "2019",
      "05",
      "16",
      ""
    )

    val validation = registerUser(Set("1000"), Date(2019, 5, 16))(form)
    validation.isValid shouldBe false

    val Invalid(errors) = validation
    val errorsSet       = errors.toSet

    errorsSet should have size 1
    errorsSet should contain(NameIsEmpty)
  }

  it should "have a valid email with domain" in {
    val form = RegistrationForm(
      "ivan",
      "ivan@",
      "passw0rd!",
      "passw0rd!",
      "2019",
      "05",
      "16",
      ""
    )

    val validation = registerUser(Set("1000"), Date(2019, 5, 16))(form)
    validation.isValid shouldBe false

    val Invalid(errors) = validation
    val errorsSet       = errors.toSet

    errorsSet should have size 1
    errorsSet should contain(InvalidEmail("ivan@"))
  }

  it should "have a valid email with user" in {
    val form = RegistrationForm(
      "ivan",
      "@mail.bg",
      "passw0rd!",
      "passw0rd!",
      "2019",
      "05",
      "16",
      ""
    )

    val validation = registerUser(Set("1000"), Date(2019, 5, 16))(form)
    validation.isValid shouldBe false

    val Invalid(errors) = validation
    val errorsSet       = errors.toSet

    errorsSet should have size 1
    errorsSet should contain(InvalidEmail("@mail.bg"))
  }

  it should "have a valid email with at sign" in {
    val form = RegistrationForm(
      "ivan",
      "ivan at mail.bg",
      "passw0rd!",
      "passw0rd!",
      "2019",
      "05",
      "16",
      ""
    )

    val validation = registerUser(Set("1000"), Date(2019, 5, 16))(form)
    validation.isValid shouldBe false

    val Invalid(errors) = validation
    val errorsSet       = errors.toSet

    errorsSet should have size 1
    errorsSet should contain(InvalidEmail("ivan at mail.bg"))
  }

  it should "have a long password" in {
    val form = RegistrationForm(
      "ivan",
      "ivan@mail.bg",
      "p0!",
      "p0!",
      "2019",
      "05",
      "16",
      ""
    )

    val validation = registerUser(Set("1000"), Date(2019, 5, 16))(form)
    validation.isValid shouldBe false

    val Invalid(errors) = validation
    val errorsSet       = errors.toSet

    errorsSet should have size 1
    errorsSet should contain(PasswordTooShort)
  }

  it should "have a strong password symbol variety" in {
    val form = RegistrationForm(
      "ivan",
      "ivan@mail.bg",
      "12345678",
      "12345678",
      "2019",
      "05",
      "16",
      ""
    )

    val validation = registerUser(Set("1000"), Date(2019, 5, 16))(form)
    validation.isValid shouldBe false

    val Invalid(errors) = validation
    val errorsSet       = errors.toSet

    errorsSet should have size 1
    errorsSet should contain(PasswordRequiresGreaterSymbolVariety)
  }

  it should "have a matching password confirmation" in {
    val form = RegistrationForm(
      "ivan",
      "ivan@mail.bg",
      "passw0rd!",
      "Passw0rd!",
      "2019",
      "05",
      "16",
      ""
    )

    val validation = registerUser(Set("1000"), Date(2019, 5, 16))(form)
    validation.isValid shouldBe false

    val Invalid(errors) = validation
    val errorsSet       = errors.toSet

    errorsSet should have size 1
    errorsSet should contain(PasswordsDoNotMatch)
  }

  it should "have a valid postal code" in {
    val form = RegistrationForm(
      "ivan",
      "ivan@mail.bg",
      "passw0rd!",
      "passw0rd!",
      "2019",
      "05",
      "16",
      "AAAA"
    )

    val validation = registerUser(Set("1000"), Date(2019, 5, 16))(form)
    validation.isValid shouldBe false

    val Invalid(errors) = validation
    val errorsSet       = errors.toSet

    errorsSet should have size 1
    errorsSet should contain(InvalidPostalCode("AAAA"))
  }

  it should "have an integer birth year" in {
    val form = RegistrationForm(
      "ivan",
      "ivan@mail.bg",
      "passw0rd!",
      "passw0rd!",
      "2K19",
      "05",
      "16",
      ""
    )

    val validation = registerUser(Set("1000"), Date(2019, 5, 16))(form)
    validation.isValid shouldBe false

    val Invalid(errors) = validation
    val errorsSet       = errors.toSet

    errorsSet should have size 1
    val error = YearIsNotAnInteger("2K19")
    errorsSet should contain(InvalidBirthdayDate(Chain(error)))
  }

  it should "have an integer birth month" in {
    val form = RegistrationForm(
      "ivan",
      "ivan@mail.bg",
      "passw0rd!",
      "passw0rd!",
      "2019",
      "five",
      "16",
      ""
    )

    val validation = registerUser(Set("1000"), Date(2019, 5, 16))(form)
    validation.isValid shouldBe false

    val Invalid(errors) = validation
    val errorsSet       = errors.toSet

    errorsSet should have size 1
    val error = MonthIsNotAnInteger("five")
    errorsSet should contain(InvalidBirthdayDate(Chain(error)))
  }

  it should "have an integer birth day" in {
    val form = RegistrationForm(
      "ivan",
      "ivan@mail.bg",
      "passw0rd!",
      "passw0rd!",
      "2019",
      "05",
      "16th",
      ""
    )

    val validation = registerUser(Set("1000"), Date(2019, 5, 16))(form)
    validation.isValid shouldBe false

    val Invalid(errors) = validation
    val errorsSet       = errors.toSet

    errorsSet should have size 1
    val error = DayIsNotAnInteger("16th")
    errorsSet should contain(InvalidBirthdayDate(Chain(error)))
  }

  it should "have a valid birth month" in {
    val form = RegistrationForm(
      "ivan",
      "ivan@mail.bg",
      "passw0rd!",
      "passw0rd!",
      "2019",
      "13",
      "16",
      ""
    )

    val validation = registerUser(Set("1000"), Date(2019, 5, 16))(form)
    validation.isValid shouldBe false

    val Invalid(errors) = validation
    val errorsSet       = errors.toSet

    errorsSet should have size 1
    val error = MonthOutOfRange(13)
    errorsSet should contain(InvalidBirthdayDate(Chain(error)))
  }

  it should "have a valid birth day" in {
    val form = RegistrationForm(
      "ivan",
      "ivan@mail.bg",
      "passw0rd!",
      "passw0rd!",
      "2019",
      "05",
      "36",
      ""
    )

    val validation = registerUser(Set("1000"), Date(2019, 5, 16))(form)
    validation.isValid shouldBe false

    val Invalid(errors) = validation
    val errorsSet       = errors.toSet

    errorsSet should have size 1
    val error = DayOutOfRange(36)
    errorsSet should contain(InvalidBirthdayDate(Chain(error)))
  }

  it should "have a valid birthday date" in {
    val form = RegistrationForm(
      "ivan",
      "ivan@mail.bg",
      "passw0rd!",
      "passw0rd!",
      "2019",
      "02",
      "31",
      ""
    )

    val validation = registerUser(Set("1000"), Date(2019, 5, 16))(form)
    validation.isValid shouldBe false

    val Invalid(errors) = validation
    val errorsSet       = errors.toSet

    errorsSet should have size 1
    val error = InvalidDate(Date(2019, 2, 31))
    errorsSet should contain(InvalidBirthdayDate(Chain(error)))
  }

  it should "have a valid birthday date in the past" in {
    val form = RegistrationForm(
      "ivan",
      "ivan@mail.bg",
      "passw0rd!",
      "passw0rd!",
      "2019",
      "5",
      "31",
      ""
    )

    val validation = registerUser(Set("1000"), Date(2019, 5, 16))(form)
    validation.isValid shouldBe false

    val Invalid(errors) = validation
    val errorsSet       = errors.toSet

    errorsSet should have size 1
    errorsSet should contain(BirthdayDateIsInTheFuture(Date(2019, 5, 31)))
  }

  "An invalid form" should "should return many errors" in {
    val form = RegistrationForm(
      "ivan",
      "ivan",
      "passw0rd!",
      "passw0rd!",
      "2K19",
      "two",
      "36",
      "AAA"
    )

    val validation = registerUser(Set("1000"), Date(2019, 5, 16))(form)
    validation.isValid shouldBe false

    val Invalid(errors) = validation
    val errorsSet       = errors.toSet

    errorsSet should have size 3
    errorsSet should contain allOf (
      InvalidEmail("ivan"),
      InvalidPostalCode("AAA")
    )

    val birthdayErrors = errorsSet.collectFirst {
      case InvalidBirthdayDate(dateErrors) => dateErrors.toSet
    }

    birthdayErrors shouldEqual Some(
      Set(
        YearIsNotAnInteger("2K19"),
        MonthIsNotAnInteger("two"),
        DayOutOfRange(36)
      )
    )
  }
}
