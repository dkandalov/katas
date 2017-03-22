package ru.sicp

def normalIncrement = {number -> number + 1}

def zero(increment, number) { number }
def one(increment, number) { increment(number) }
def two(increment, number) { increment(increment(number)) }
def addOne(functionalNumberReference, increment) {
  { number -> increment(functionalNumberReference(number)) }
}
/*def plus(Arithmetic arithmetic, funcNumberRef1, funcNumberRef2) {
	funcNumberRef1(arithmetic,
		funcNumberRef2(arithmetic, arithmetic.zeroNumber)
	)
}*/

def zeroReference = this.&zero
def oneReference = this.&one
def twoReference = this.&two
def addOneReference = this.&addOne

def f = addOne(twoReference.curry(twoReference.curry(normalIncrement)), normalIncrement)(0)
println(f)
//def f2 = addOne(twoReference,
//		new Arithmetic(0, twoReference.curry(
//				new Arithmetic(0, twoReference.curry(normalArithmetic))
//		))
//)
//println(f2)

//assert plus(normalArithmetic, zeroReference, zeroReference) == 0
//assert plus(normalArithmetic, zeroReference, oneReference) == 1
//assert plus(normalArithmetic, oneReference, oneReference) == 2
//assert plus(normalArithmetic, oneReference, twoReference) == 3
//assert plus(normalArithmetic, twoReference, twoReference) == 4
assert addOne(zeroReference, normalIncrement)(0) == 1
assert addOne(oneReference, normalIncrement)(0) == 2
assert addOne(twoReference, normalIncrement)(0) == 3

assert zero(normalIncrement, 0) == 0
assert zero(normalIncrement, 1) == 1
assert zero(normalIncrement, 2) == 2
assert one(normalIncrement, 0) == 1
assert one(normalIncrement, 1) == 2
assert one(normalIncrement, 2) == 3
assert two(normalIncrement, 0) == 2
assert two(normalIncrement, 1) == 3
assert two(normalIncrement, 2) == 4
