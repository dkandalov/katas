import Foundation

extension String {
    func printed() {
        print(self)
    }
}
func greeting(message name: String = "you") -> String {
    return "Hello, \(name)!"
}

greeting().printed()
greeting(message: "World").printed()
