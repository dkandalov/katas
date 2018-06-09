import XCTest

class FactorialTests: XCTestCase {

    func factorial(n: Decimal) -> Decimal {
        if n == 0 {
            return 1
        } else {
            return factorial(n: n - 1) * n
        }
    }

    func testExample() {
        XCTAssertEqual(factorial(n: 0), 1)
        XCTAssertEqual(factorial(n: 1), 1)
        XCTAssertEqual(factorial(n: 2), 2)
        XCTAssertEqual(factorial(n: 3), 6)
        XCTAssertEqual(factorial(n: 4), 24)
        XCTAssertEqual(factorial(n: 5), 120)
        // XCTAssertEqual(factorial(n: 30), 265252859812191078400000000000000.0)
        // XCTAssertEqual(factorial(n: 1000), 265252859812191058636308480000000.0)
    }
}
