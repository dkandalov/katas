import Foundation
import XCTest

class DataMungingTests: XCTestCase {
    func testFindDayWithMinTemperatureSpread() throws {
        let text = try NSString(
            contentsOfFile: "/Users/dima/IdeaProjects/katas/swift/hello/Tests/weather.dat",
            encoding: String.Encoding.ascii.rawValue
        )
        print(text)
    }
}
