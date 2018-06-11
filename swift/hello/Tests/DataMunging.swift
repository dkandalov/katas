import Foundation
import XCTest

class DataMungingTests: XCTestCase {

    struct DayEntry : Equatable {
        let day: Int
        let maxTemp: Int
        let minTemp: Int

        func tempSpread() -> Int {
            return maxTemp - minTemp
        }

        static func ==(lhs: DayEntry, rhs: DayEntry) -> Bool {
            return lhs.day == rhs.day && lhs.maxTemp == rhs.maxTemp && lhs.minTemp == rhs.minTemp
        }
    }

    struct TeamEntry : Equatable {
        let name: String
        let goalsFor: Int
        let goalsAgainst: Int

        func goalDifference() -> Int {
            return abs(goalsFor - goalsAgainst)
        }

        static func ==(lhs: TeamEntry, rhs: TeamEntry) -> Bool {
            return lhs.name == rhs.name && lhs.goalsFor == rhs.goalsFor && lhs.goalsAgainst == rhs.goalsAgainst
        }
    }

    func testFindDayWithMinTemperatureSpread() throws {
        let text = try NSString(
            contentsOfFile: "/Users/dima/IdeaProjects/katas/swift/hello/Tests/weather.dat",
            encoding: String.Encoding.ascii.rawValue
        )
        let minEntry = text
            .components(separatedBy: "\n")
            .dropFirst(8).dropLast(2)
            .map { (line: String) in
                let regex = try! NSRegularExpression(pattern: " +", options: NSRegularExpression.Options.caseInsensitive)
                let modString = regex.stringByReplacingMatches(in: line, options: NSRegularExpression.MatchingOptions.reportProgress, range: NSMakeRange(0, line.count), withTemplate: " ")
                return modString.components(separatedBy: " ")
            }
            .map { (values: [String]) in
                values[1...3].map {
                    $0.replacingOccurrences(of: "*", with: "")
                }
            }
            .map { (values: [String]) in
                DayEntry(day: Int(values[0])!, maxTemp: Int(values[1])!, minTemp: Int(values[2])!)
            }
            //.sorted(by: { $0[1] - $0[2] < $1[1] - $1[2] }) // Error: expression was too complex to be solved in reasonable time; consider breaking up the expression into distinct sub-expressions
            .min(by: { (entry1, entry2) in
                entry1.tempSpread() < entry2.tempSpread()
            })!

        XCTAssertEqual(minEntry, DayEntry(day: 14, maxTemp: 61, minTemp: 59))
    }

    func testTeamWithMinGoalDifference() throws {
        let text = try NSString(
            contentsOfFile: "/Users/dima/IdeaProjects/katas/swift/hello/Tests/football.dat",
            encoding: String.Encoding.ascii.rawValue
        )
        let minEntry = text
            .components(separatedBy: "\n")
            .dropFirst(5).dropLast(1)
            .filter({ (line: String) in !line.contains("-----") })
            .map { (line: String) in
                let regex = try! NSRegularExpression(pattern: " +", options: NSRegularExpression.Options.caseInsensitive)
                let modString = regex.stringByReplacingMatches(in: line, options: NSRegularExpression.MatchingOptions.reportProgress, range: NSMakeRange(0, line.count), withTemplate: " ")
                return modString.components(separatedBy: " ")
            }
            .map { (values: [String]) in
                TeamEntry(name: values[2], goalsFor: Int(values[7])!, goalsAgainst: Int(values[9])!)
            }
            .min(by: { (entry1: TeamEntry, entry2: TeamEntry) in
                entry1.goalDifference() < entry2.goalDifference()
            })!

        XCTAssertEqual(minEntry, TeamEntry(name: "Aston_Villa", goalsFor: 46, goalsAgainst: 47))
    }
}
