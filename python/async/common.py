def expect_to_be_equal(actual, expected):
    if actual != expected:
        raise Exception("Expected: " + str(expected) + "\nbut was: " + str(actual))