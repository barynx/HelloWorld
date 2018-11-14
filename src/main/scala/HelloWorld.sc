//functions
def abs (x: Double) = if (x<0) -x else x

def sqrIter (guess: Double, x: Double): Double =
  if (isGoodEnough(guess, x)) guess
  else sqrIter (improve(guess, x), x)

def isGoodEnough(guess: Double, x: Double) = abs (guess * guess - x) < 0.001

def improve (guess: Double, x: Double ) =
  (guess + x / guess) / 2


def sqrt (x: Double) =
  sqrIter (5, x)

def c = sqrt(4)

def sum (f: Int => Int): (Int, Int) => Int = {

  def sumF (a: Int, b: Int): Int =
    if (a>b) 0
    else f(a) + sumF (a+1, b)
  sumF
}

def sumCubes = sum(cube)
def cube (x: Int) = x * x * x

sumCubes(1,8)
sum(cube)
sum(x=>x*x*x)(1,8)

def sum2 (f: Int => Int)(a: Int, b: Int): Int =
if (a>b) 0 else f(a) + sum2(f)(a+1, b)

sum2(cube)(1,8)

def mapReduce (f: Int => Int, zero: Int, combine: (Int, Int)=>Int)(a: Int, b: Int): Int =
  if (a>b) zero
  else combine(f(a), mapReduce(f,zero, combine)(a+1, b))

mapReduce (x =>x*x,1,(x,y)=> x*y)(1,3)


//case classes
case class Number(n: Int) extends Expr
case class Sum(x: Expr, y: Expr) extends Expr

trait Expr
{
  def eval: Int = this match {
    case Number(n) => n
    case Sum(x,y) => x.eval + y.eval
  }

  def show: String = this match {
    case Number(n) => n.toString
    case Sum(x,y) => "("+ x.show + "+" + y.show + ")"
  }
}

Sum(Sum(Number(1),Number(3)),Number(2)).show

//lists
def fruits = List("grapes", "bananas", "apples")
def vegetables = List("pumpkins", "tomatoes", "cabbage", "corn" )
fruits.head
fruits.tail
fruits.last
fruits.indexOf("apple")

def fruitsCons = "apples" :: "oranges" :: List()

fruits ::: vegetables
fruits ++ vegetables

fruits(2)

fruits drop 2

//pair and tuples
val pair = ("answer", 42)

val (label, value) = pair

//HO list functions

def scaleList (xs: List[Double], factor: Double) =
  xs map (x => x * factor)

def doubles = List(-1.0, -2.2, 3.3, 4.8)
def index = 1 until 3

scaleList(doubles, 2)

def chujList (xs: List[String], ys: Range) =
  (xs) map (x => (x,ys))

chujList(fruits, index)

def posElements (xs: List[Double]) =
  xs filter (x => x > 0)

def negElements (xs: List[Double]) =
  xs filterNot (x => x > 0)

def partElements (xs: List[Double]) =
  xs partition (x => x > 0)

posElements(doubles)
negElements(doubles)
partElements(doubles)

def pack[T](xs: List[T]) : List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs1 =>
    val (first, rest) = xs span (y => y == x)
    first :: pack(rest)
}

def letters = List("a","a","a","b","b","c","c","c")

pack(letters)

def encode[T] (xs: List[T]) : List[(T,Int)] =
  pack(xs) map (elem => (elem.head,elem.length))

encode(letters)

//reduction of lists

def sumList(xs: List[Double]) = (0.0 :: xs) reduceLeft (_+_)
sumList(doubles)

def sumListFold(xs: List[Double]) = (xs foldLeft 0.0)(_+_)
sumListFold(doubles)

def foldStrings(xs: List[String])= (xs foldLeft "")(_+ " " + _)
foldStrings(fruits)

// other collections
val pets = Vector("Kopi", "Karbon", "Bizzy", "Weendy", "Fifi")

pets partition(x => x.startsWith("K"))
pets filter(x => x.startsWith("K"))

pets :+ "Patyczak"
"Patyczak" +: pets

val petsArr = Array("Kopi", "Karbon", "Bizzy", "Weendy", "Fifi")
petsArr.tail

val r: Range = 1 to 20 by 3
val t = 1 to 20 by 2

r partition ( x => x>1)

pets exists (c => c.equals("Kopi"))
pets forall (c => c.equals("Kopi"))

val pairs = fruits zip pets
pairs.unzip._1
pairs.unzip._2

//for expressions
case class Person(name: String, age: Int)
val persons = Vector[Person](Person("Bobby", 1), Person("Karol", 2), Person("Maggie", 21))

for (p <- persons if p.age >20) yield p.name

val v = Vector(1,2,4,8,16,32)
val u = Vector(3,9,27,60,34)

(for ((x,y) <- v zip u) yield x*y).sum
(for ((x,y) <- v zip u) yield x+y).product

//Sets

val exampleSet = (1 to 5).toSet.toVector
val exampleSet2 = (1 to 20 by 2).toVector
val exampleSet3 = (1 to 60 by 2).toArray
val exampleSet4 = (1 to 60 by 2).toArray

//Maps
val romanNumerals = Map("I" -> 1, "V" -> 5, "X" ->10)

//Sorted and GroupBy


// sum() returns a function that takes two integers and returns an integer
def sumCS(f: Int => Int): (Int, Int) => Int = {
  def sumf(a: Int, b: Int): Int = { if(a < b) f(a) + sumf(a+1, b) else f(a) }
  sumf
}

def cubeCS(x: Int): Int = x * x * x