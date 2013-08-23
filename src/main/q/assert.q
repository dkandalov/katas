expect:{[actual;matcher]
    $[matcher[`match][actual];;show matcher[`describeFailure][actual]]}

newEqualMatcher:{[expected]
    `match`describeFailure ! (
        {[e;actual] e = actual}[expected];
        {[e;actual] "Expected: '" , (string e) , "' but was: '" , (string actual) , "'"}[expected] )}
toEqual:{ [expected]
    newEqualMatcher[expected] }

/ show (newEqualMatcher[123][`match][123])
/ show (newEqualMatcher[123][`describeFailure][234])
/ expect[1; toEqual[1]]
/ expect[1; toEqual[0]]



