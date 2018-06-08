import XCTest

class FactorialTests: XCTestCase {

    func factorial(n: Int) -> Int {
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
//        XCTAssertEqual(factorial(n: 30), 120)
    }
}
