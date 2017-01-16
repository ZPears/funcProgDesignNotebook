// say you have two functions, iterate and square:

def iterate[T](n: Int, f: T => T, x: T): T =
  if (n == 0) x else iterate(n-1, f, f(x))

def square(x: Int) = x * x

iterate(1, square, 3)

// nice definition:
// an object HAS STATE if its behavior is influenced by its history.

class BankAccount {
  private var balance = 0
  def deposit(amount: Int): Unit = {
    require(amount > 0, "Can't deposit 0 or less!!")
    balance = balance + amount
  }
  def withdraw(amount: Int): Int = {
    require(amount > 0, "Can't withdraw 0 or less!!")
    require(amount < balance, "Not enough money!!")
    balance = balance - amount
    balance
  }
}

val account = new BankAccount

account deposit 50
account deposit 20
account withdraw 40
account withdraw 50
