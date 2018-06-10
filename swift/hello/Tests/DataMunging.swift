import Foundation
import XCTest

class DataMungingTests: XCTestCase {
    func testFindDayWithMinTemperatureSpread() throws {
        let text = try NSString(
            contentsOfFile: "/Users/dima/IdeaProjects/katas/swift/hello/Tests/weather.dat",
            encoding: String.Encoding.ascii.rawValue
        )
        text
            .components(separatedBy: "\n")
            .dropFirst(8).dropLast(2)
            .map { (line: String) in
                let regex = try! NSRegularExpression(pattern: " +", options: NSRegularExpression.Options.caseInsensitive)
                let modString = regex.stringByReplacingMatches(in: line, options: NSRegularExpression.MatchingOptions.reportProgress, range: NSMakeRange(0, line.count), withTemplate: " ")
                return modString.components(separatedBy: " ")
            }
            .map { (values: [String]) in
                values[1...3].map {
                    Int($0.replacingOccurrences(of: "*", with: ""))
                }
            }
            .forEach {
                print($0)
            }
    }
}
